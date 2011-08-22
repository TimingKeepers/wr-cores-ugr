library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;              -- for gc_crc_gen
use work.endpoint_private_pkg.all;
use work.ep_wbgen2_pkg.all;

-- 3rd deframing pipeline stage - VLAN Unit

entity ep_rx_vlan_unit is
  port(clk_sys_i : in std_logic;
       rst_n_i   : in std_logic;

       snk_fab_i  : in  t_ep_internal_fabric;
       snk_dreq_o : out std_logic;

       src_fab_o  : out t_ep_internal_fabric;
       src_dreq_i : in  std_logic;

       tclass_o       : out std_logic_vector(2 downto 0);
       tclass_valid_o : out std_logic;

       rmon_o : inout t_rmon_triggers;
       regs_b : inout t_ep_registers
       );

end ep_rx_vlan_unit;

architecture behavioral of ep_rx_vlan_unit is

  
  type t_tag_type is (NONE, PRIO, VLAN, NULL_VLAN);
  type t_state is (WAIT_FRAME, DATA, DISCARD_FRAME, INSERT_TAG);

  signal dreq_mask  : std_logic;
  signal hdr_offset : std_logic_vector(11 downto 0);

  signal comb_tag_type : t_tag_type;

  signal tag_type : t_tag_type;
  signal state    : t_state;

  signal at_ethertype     : std_logic;
  signal at_vid           : std_logic;
  signal at_tpid          : std_logic;
  signal is_tagged        : std_logic;
  signal stored_ethertype : std_logic_vector(15 downto 0);

  signal prio_int : std_logic_vector(2 downto 0);


  procedure f_vlan_decision
    (tag_type       :     t_tag_type;
     qmode          : in  std_logic_vector(1 downto 0);
     admit          : out std_logic;
     use_pvid       : out std_logic;
     use_fixed_prio : out std_logic) is
  begin

    use_pvid       := 'X';
    use_fixed_prio := 'X';
    admit          := '0';

    -- From Jose's table. Thanks a lot!
    case (qmode) is
      when c_QMODE_PORT_ACCESS =>
        case tag_type is
          when NONE =>
            admit := '1'; use_pvid := '1'; use_fixed_prio := '1';
          when PRIO =>
            admit := '1'; use_pvid := '1'; use_fixed_prio := '0';
          when VLAN =>
            admit := '0'; use_pvid := '0';
          when NULL_VLAN =>
            admit := '0';
        end case;

      when c_QMODE_PORT_TRUNK =>
        case tag_type is
          when NONE =>
            admit := '0';
          when PRIO=>
            admit := '0';
          when VLAN=>
            admit          := '1';
            use_pvid       := '0';
            use_fixed_prio := '0';
          when NULL_VLAN =>
            admit := '0';
        end case;

      when c_QMODE_PORT_UNQUALIFIED =>
        case tag_type is
          when NONE =>
            admit    := '1';
            use_pvid := '1'; use_fixed_prio := '1';
          when PRIO=>
            admit    := '1';
            use_pvid := '1'; use_fixed_prio := '0';
          when VLAN=>
            admit    := '1';
            use_pvid := '0'; use_fixed_prio := '0';
          when NULL_VLAN =>
            admit := '0';
        end case;
      when others => null;
    end case;
  end procedure;

  
  
  
  
  
begin  -- behavioral

  at_ethertype <= hdr_offset(5) and snk_fab_i.dvalid and src_dreq_i;
  at_tpid      <= hdr_offset(6) and snk_fab_i.dvalid and is_tagged and src_dreq_i;
  at_vid       <= hdr_offset(7) and snk_fab_i.dvalid and is_tagged and src_dreq_i;

  regs_b <= c_ep_registers_init_value;


  snk_dreq_o <= src_dreq_i and dreq_mask;


  p_decode_tag_type : process(snk_fab_i, is_tagged)
  begin
    if(is_tagged = '0') then
      comb_tag_type <= NONE;
    else
      case snk_fab_i.data(11 downto 0) is
        when x"000" => comb_tag_type <= PRIO;
        when x"fff" => comb_tag_type <= NULL_VLAN;
        when others => comb_tag_type <= VLAN;
      end case;
    end if;
  end process;

  p_tag_untag : process(clk_sys_i)
    variable admit, use_pvid, use_fixed_prio : std_logic;
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' or regs_b.ecr_rx_en_o = '0' then
        hdr_offset(hdr_offset'left downto 1) <= (others => '0');
        hdr_offset(0)                        <= '1';
        state                                <= WAIT_FRAME;
        dreq_mask                            <= '0';
      else

        if(snk_fab_i.error = '1') then
          state <= DISCARD_FRAME;
        else

          case state is
            when WAIT_FRAME =>
              dreq_mask       <= '1';
              src_fab_o.eof   <= '0';
              src_fab_o.error <= '0';

              if(snk_fab_i.sof = '1') then
                hdr_offset(hdr_offset'left downto 1) <= (others => '0');
                hdr_offset(0)                        <= '1';
                state                                <= DATA;
              end if;

            when DATA =>

              dreq_mask <= '1';

              -- pass the data through the pipeline
              src_fab_o.data   <= snk_fab_i.data;
              src_fab_o.dvalid <= snk_fab_i.dvalid;

              if(at_ethertype = '1') then
                if(snk_fab_i.data = x"8100") then  -- got a 802.1q tagged frame
                  is_tagged <= '1';
                else
                  is_tagged <= '0';
                  -- if the packet has to be VLAN-tagged on ingress, do it as
                  -- early as possible (i.e. right after identifying the Ethertype)
                  -- to avoid increasing latency
                  if(regs_b.vcr0_qmode_o = c_QMODE_PORT_ACCESS) then
                    -- fix to Jose's table: UNQUALIFIED ports don't tag untagged
                    -- frames with PVID
                    src_fab_o.dvalid <= '0';
                    src_fab_o.data   <= x"8100";
                    prio_int         <= regs_b.vcr0_prio_val_o;
                    state            <= INSERT_TAG;
                    dreq_mask        <= '0';
                  end if;
                end if;

                -- remember the Ethertype, as it is needed for producing an
                -- 802.1q header
                stored_ethertype <= snk_fab_i.data;
              end if;

              -- we are at the VID field in the header. Most of the VLAN logic lives
              -- here :)
              if(at_vid = '1') then

                -- decide what to do with the frame, basing on the port mode
                -- (ACCESS, TRUNK, UNQUALIFIED), and whether the frame is tagged
                -- or not.
                f_vlan_decision(comb_tag_type, regs_b.vcr0_qmode_o, admit, use_pvid, use_fixed_prio);

                if(admit = '0') then    -- oops...
                  state <= DISCARD_FRAME;
                end if;

                -- assign the VID
                if(use_pvid = '1')then
                  src_fab_o.data(11 downto 0) <= regs_b.vcr0_pvid_o;
                end if;


                -- assign the priority 
                if(regs_b.vcr0_fix_prio_o = '1' or use_fixed_prio = '1') then
                  -- Forced priority (or a non-priority tagged frame)? Take the priority
                  -- value from VCR0 register
                  prio_int <= regs_b.vcr0_prio_val_o;
                else
                  -- Got a priority tag - use the value from the VLAN tag
                  prio_int <= snk_fab_i.data(15 downto 13);
                end if;
              end if;

              if(snk_fab_i.eof = '1') then
                state         <= WAIT_FRAME;
                src_fab_o.eof <= '1';
              end if;

              if(snk_fab_i.dvalid = '1') then
                hdr_offset <= hdr_offset(hdr_offset'left-1 downto 0) & '0';
              end if;


            when INSERT_TAG =>
              src_fab_o.dvalid <= '0';

              if(src_dreq_i = '1') then
-- we are at 7th word from the beginning of the frame, but the sink reception
-- is disabled, so we can insert the original ethertype as the TPID
                if(hdr_offset(6) = '1') then
                  src_fab_o.data   <= stored_ethertype;
                  src_fab_o.dvalid <= '1';
                end if;

                if(hdr_offset(7) = '1') then
                  src_fab_o.data   <= regs_b.vcr0_prio_val_o & '0' & regs_b.vcr0_pvid_o;
                  state            <= DATA;
                  src_fab_o.dvalid <= '1';
                end if;

                hdr_offset <= hdr_offset(hdr_offset'left-1 downto 0) & '0';
              end if;

            when DISCARD_FRAME =>
              if(src_dreq_i = '1') then
                src_fab_o.error <= '1';
                state           <= WAIT_FRAME;
              end if;
          end case;
        end if;
      end if;
    end if;
  end process;


  -- Process: p_map_prio_to_tc
  -- Maps the PCP value from the 802.1q header into a traffic class for further
  -- processing. The mapping table is stored in TCAR register.
  p_map_prio_to_tc : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0' or regs_b.ecr_rx_en_o = '0' or snk_fab_i.sof = '1')then
        tclass_valid_o <= '0';
      elsif(hdr_offset(8) = '1') then
        -- we're already after the headers, so prio_int is
        -- certainly valid
        tclass_valid_o <= '1';
        case prio_int is
          when "000"  => tclass_o <= regs_b.tcar_pcp_map_o(2 downto 0);
          when "001"  => tclass_o <= regs_b.tcar_pcp_map_o(5 downto 3);
          when "010"  => tclass_o <= regs_b.tcar_pcp_map_o(8 downto 6);
          when "011"  => tclass_o <= regs_b.tcar_pcp_map_o(11 downto 9);
          when "100"  => tclass_o <= regs_b.tcar_pcp_map_o(14 downto 12);
          when "101"  => tclass_o <= regs_b.tcar_pcp_map_o(17 downto 15);
          when "110"  => tclass_o <= regs_b.tcar_pcp_map_o(20 downto 18);
          when "111"  => tclass_o <= regs_b.tcar_pcp_map_o(23 downto 21);
          when others => tclass_o <= "XXX";  -- packet probably contains porn
        end case;
      end if;
    end if;
  end process;

  src_fab_o.sof    <= regs_b.ecr_rx_en_o and snk_fab_i.sof;

end behavioral;




