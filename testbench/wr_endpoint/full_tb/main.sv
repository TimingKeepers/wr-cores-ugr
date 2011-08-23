`timescale 1ns/1ps

`include "endpoint_regs.v"
`include "old_endpoint_regs.v"
`include "endpoint_mdio.v"
`include "wishbone_test_master.v"
`include "tbi_utils.sv"
`include "fabric_emu/fabric_emu.sv"

`include "if_wb_master.svh"
`include "if_wb_slave.svh"
`include "wb_packet_source.svh"
`include "wb_packet_sink.svh"

`timescale 1ps/1ps

`define WB_USE_EXTERNAL_CLOCK		       



`define EP_QMODE_ACCESS 0
`define EP_QMODE_TRUNK 1
`define EP_QMODE_UNQ 3

const int c_RBCLK_PERIOD   = 8010;
const int c_REFCLK_PERIOD  = 8000;


module old_endpoint_test_wrapper
  (
   input clk_sys_i,
   input clk_ref_i,
   input clk_rx_i,
   input rst_n_i,

   output [9:0] td_o,
   input [9:0] rd_i
   );

   `WRF_FULL_WIRES(toEP)
   `WRF_FULL_WIRES(fromEP)
   
   wire txtsu_valid, txtsu_ack;
   wire [4:0] txtsu_pid;
   wire [15:0] txtsu_fid;
   wire [31:0] txtsu_timestamp;

   
   
   wire[7:0] gtx_data;
   wire gtx_k;
   wire gtx_disparity;
   wire gtx_enc_error;
   wire [7:0] grx_data;
   wire grx_clk;
   wire grx_k;
   wire grx_enc_error;
   wire [3:0] grx_bitslide;
   wire gtp_rst;

   CWishboneAccessor WB;
     
   IWishboneMaster 
     #(
       .g_data_width(32),
       .g_addr_width(7))
   U_WB
     (
      .clk_i(clk_sys_i),
      .rst_n_i(rst_n_i)
      );

   wr_tbi_phy U_Phy 
     (
      .serdes_rst_i (gtp_rst),
      .serdes_loopen_i(1'b0),
      .serdes_prbsen_i(1'b0),
      .serdes_enable_i(1'b1), 
      .serdes_syncen_i(1'b1),

      .serdes_tx_data_i  (gtx_data),
      .serdes_tx_k_i     (gtx_k),
      .serdes_tx_disparity_o (gtx_disparity),
      .serdes_tx_enc_err_o   (gtx_enc_error),

      .serdes_rx_data_o      (grx_data),
      .serdes_rx_k_o        (grx_k),
      .serdes_rx_enc_err_o  (grx_enc_error),
      .serdes_rx_bitslide_o (grx_bitslide),


      .tbi_refclk_i (clk_ref_i),
      .tbi_rbclk_i  (clk_rx_i),

      .tbi_td_o     (td_o),
      .tbi_rd_i     (rd_i),
      .tbi_syncen_o (),
      .tbi_loopen_o (),
      .tbi_prbsen_o (),
      .tbi_enable_o ()
      );



   
   
   fabric_emu EMU
      (
       .clk_i(clk_sys_i),
       .rst_n_i(rst_n_i),
    
       `WRF_FULL_CONNECT_SOURCE(rx, toEP),
       `WRF_FULL_CONNECT_SINK(tx, fromEP),

       .txtsu_port_id_i(txtsu_pid),
       .txtsu_fid_i (txtsu_fid),
       .txtsu_tsval_i  (txtsu_timestamp),
       .txtsu_valid_i (txtsu_valid),
       .txtsu_ack_o (txtsu_ack)
       );


     task mdio_write(int addr, int data);
        uint64_t rval;
        

      $display("MDIO_write %x %x", addr, data);
      
      WB.write(`OLD_ADDR_EP_MDIO_CR, ((addr >> 2) << 16) | `OLD_EP_MDIO_CR_RW | data);
      while(1)begin
	 WB.read(`OLD_ADDR_EP_MDIO_SR, rval);
	 if(rval & `OLD_EP_MDIO_SR_READY) break;
      end
   endtask // mdio_write

   task mdio_read(int addr, output int data);
        uint64_t rval;
      
      WB.write(`OLD_ADDR_EP_MDIO_CR, (addr >> 2));
      while(1)begin
	 WB.read(`OLD_ADDR_EP_MDIO_SR, rval);
	 if(rval & `OLD_EP_MDIO_SR_READY) begin
	    data  = rval[15:0];
	    break;
	 end
      end
   endtask // mdio_read
  
   task initialize_EP_regs();
    
      $display("Initializing EP registers...");
    
      WB  = U_WB.get_accessor();
      WB.set_mode(CLASSIC);
  
      WB.write(`OLD_ADDR_EP_ECR, `OLD_EP_ECR_RX_EN_FRA | `OLD_EP_ECR_TX_EN_FRA);
      WB.write(`OLD_ADDR_EP_RFCR, 3 << `OLD_EP_RFCR_QMODE_OFFSET); // QMODE = UNQUALIFIED
      WB.write(`OLD_ADDR_EP_MACH, 'haabb);  // assign a dummy MAC address
      WB.write(`OLD_ADDR_EP_MACL, 'hccddeeff);
      WB.write(`OLD_ADDR_EP_TSCR, `OLD_EP_TSCR_EN_RXTS);

  //    mdio_write(`ADDR_MDIO_MCR, `MDIO_MCR_RESET);
      
   endtask // initialize_EP_regs


   old_wrsw_endpoint
     #(
       .g_phy_mode("GTP"),
       .g_simulation(1)
       )
     DUT (
	  .clk_ref_i(clk_ref_i),
	  .clk_sys_i(clk_sys_i),
	  .clk_dmtd_i(clk_ref_i),
	  .rst_n_i (rst_n_i),
	  .pps_csync_p1_i(1'b0),
	  
	  `_WRF_CONNECT_MANDATORY_SOURCE(rx, fromEP),
	  `_WRF_CONNECT_MANDATORY_SINK(tx, toEP),

	  .rtu_full_i(1'b0),
	  .rtu_almost_full_i(1'b0),
	  
	  .wb_cyc_i  (U_WB.master.cyc),
	  .wb_stb_i  (U_WB.master.stb),
	  .wb_we_i   (U_WB.master.we),
	  .wb_sel_i  (U_WB.master.sel),
	  .wb_addr_i (U_WB.master.adr [5:0]),
	  .wb_data_i (U_WB.master.dat_o),
	  .wb_data_o (U_WB.master.dat_i),
	  .wb_ack_o  (U_WB.master.ack),
	  
	  .txtsu_port_id_o(txtsu_pid),
	  .txtsu_frame_id_o (txtsu_fid),
	  .txtsu_tsval_o  (txtsu_timestamp),
	  .txtsu_valid_o (txtsu_valid),
	  .txtsu_ack_i (txtsu_ack),

// GTP I/F
          .gtp_rst_o(gtp_rst),
	  .gtp_tx_clk_i(clk_ref_i),
	  .gtp_tx_data_o (gtx_data),
	  .gtp_tx_k_o    (gtx_k),
	  .gtp_tx_disparity_i (gtx_disparity),
	  .gtp_tx_enc_err_i  (gtx_enc_error),
	
	  .gtp_rx_data_i     (grx_data),
	  .gtp_rx_clk_i    (clk_rx_i),
	  .gtp_rx_k_i        (grx_k),
	  .gtp_rx_enc_err_i  (grx_enc_error),
	  .gtp_rx_bitslide_i (grx_bitslide)
    );

   
   

   // sets the Q-mode of the endpoint
   task ep_set_qmode(int qmode);
      uint64_t rfcr;

      string s;
      
      case (qmode)
	`EP_QMODE_ACCESS: s="ACCESS"; 
	`EP_QMODE_TRUNK: s="TRUNK";
	`EP_QMODE_UNQ: s="UNQUALIFIED";
      endcase // case (qmode)
      
      
      $display("Setting qmode to: %s", s);
      
      WB.read(`OLD_ADDR_EP_RFCR, rfcr);
      rfcr  = rfcr & (~`OLD_EP_RFCR_QMODE);
      rfcr  = rfcr | ( qmode << `OLD_EP_RFCR_QMODE_OFFSET);
      WB.write(`OLD_ADDR_EP_RFCR, rfcr);

     endtask // ep_set_qmode
   

   
   initial $dumpfile("dump.vcd");
   initial $dumpvars(0, main);

   reg ready  = 0;

   always@(rst_n_i) if(!rst_n_i)
     ready    = 0;
   
   
   initial begin

      @(posedge rst_n_i);
      @(posedge clk_sys_i);

      // configure some EP registers
      initialize_EP_regs();

      ready  = 1;
   end
   

   EthPacket rx_queue[$];

class WRFPacketSink extends EthPacketSink;
   
   function int poll();
      return rx_queue.size();
   endfunction // poll
   
   task recv(ref EthPacket pkt, ref int result = _null);
      while(!poll()) @(posedge clk_sys_i);
      pkt     = rx_queue.pop_front();
      result  = 1;
   endtask // recv
   
endclass // WRFPacketSink
   
   WRFPacketSink sink;

class WRFPacketSource extends EthPacketSource;
   task send(ref EthPacket pkt, ref int result = _null);
      byte tmp[];

      pkt.serialize(tmp);

      EMU.send_raw(tmp);
      
   endtask // send
endclass // WRFPacketSource
   

   WRFPacketSource src;
   
   
   
   initial 
     begin
        sink  = new;
        src   = new;
        
        
        forever begin
	@(posedge clk_sys_i);
	
        if(EMU.rx_queue != null && EMU.rx_queue.get_count())
          begin
	     automatic int i;
	     automatic ether_frame_t fra;
             automatic EthPacket pkt;

             pkt= new;
             
	     EMU.rx_queue.pop(fra);
//	     $display("RxFrame!");
             pkt.deserialize(fra.raw_data);
//             pkt.dump();
             rx_queue.push_back(pkt);
             
             end
        end
   end
   
endmodule

/* Packet Filter microcode definitions */

class PFilterMicrocode;
   typedef enum 
    {
     AND = 0,
     NAND = 4,
     OR = 1,
     NOR = 5,
     XOR = 2,
     XNOR = 6,
     MOV = 3,
     NOT = 7
     } pfilter_op_t;


   const uint64_t PF_MODE_LOGIC    = (1<<34);
   const uint64_t PF_MODE_CMP    = 0;

   const int max_size         = 64;
   
   protected int code_pos;
   protected uint64_t code_buf[];

   function new();
      code_pos  = 0;
      code_buf  = new[max_size];
   endfunction // new

   task check_size();
      if(code_pos == max_size - 1)
        $error("microcode: code too big (max size: %d)", max_size);
   endtask // check_size

   task check_reg_range(int val, int minval, int maxval, string name);
      if(val < minval || val > maxval)
        $error("microcode: %s register out of range (%d to %d)", name, minval,maxval);
   endtask // check_reg_range
   
   // rd = (packet[offset] & mask == value) op rd
   task cmp(int offset, int value, int mask, pfilter_op_t op, int rd);
      uint64_t ir;

      check_size();
      
      if(offset > code_pos-1)
        $error("microcode: comparison offset is bigger than current PC. Insert some nops before comparing");

      check_reg_range(rd, 1, 15, "ra/rd");
   
      ir  = (PF_MODE_CMP | (offset << 7)
            | ((mask & 'h1) ? (1<<29) : 0)
            | ((mask & 'h10) ? (1<<30) : 0)
            | ((mask & 'h100) ? (1<<31) : 0)
            | ((mask & 'h1000) ? (1<<32) : 0))
        | op | (rd << 3);

      ir                    = ir | (value & 'hffff) << 13;

      code_buf[code_pos++]  = ir;
   endtask // cmp


   // rd                    = (packet[offset] & (1<<bit_index)) op rd
   task btst(int offset, int bit_index, pfilter_op_t op, int rd);
      uint64_t ir;

      check_size();
      
      if(offset > code_pos-1)
        $error("microcode: comparison offset is bigger than current PC. Insert some nops before comparing");

      check_reg_range(rd, 1, 15, "ra/rd");
      check_reg_range(bit_index, 0, 15, "bit index");
   
      ir                    = ((1<<33) | PF_MODE_CMP | (offset << 7) | (bit_index << 29) | op | (rd << 3));
      
      code_buf[code_pos++]  = ir;
   endtask // cmp
         
   task nop();
      uint64_t ir;
      check_size();
      ir  = PF_MODE_LOGIC;
      code_buf[code_pos++]  = ir;
   endtask // nop
         

   // rd  = ra op rb
   task logic2(int rd, int ra, pfilter_op_t op, int rb);
      uint64_t ir;
      check_size();
      check_reg_range(ra, 0, 31, "ra");
      check_reg_range(rb, 0, 31, "rb");
      check_reg_range(rd, 1, 31, "rd");

      ir  = (ra << 8) | (rb << 13) | ((rd & 'hf) << 3) | ((rd & 'h10) ? (1<<7) : 0) | op;
      ir  = ir | PF_MODE_LOGIC | (3<<23);
      code_buf[code_pos++]  = ir;
   endtask // logic2

   // rd  = (ra op rb) op2 rc
   task logic3(int rd, int ra, pfilter_op_t op, int rb, pfilter_op_t op2, int rc);
      uint64_t ir;
      check_size();
      check_reg_range(ra, 0, 31, "ra");
      check_reg_range(rb, 0, 31, "rb");
      check_reg_range(rc, 0, 31, "rb");
      check_reg_range(rd, 1, 31, "rd");

      ir  = (ra << 8) | (rb << 13) | (rc << 18) | ((rd & 'hf) << 3) | ((rd & 'h10) ? (1<<7) : 0) | op;
      ir  = ir | PF_MODE_LOGIC | (op2<<23);
      code_buf[code_pos++]  = ir;
   endtask // logic3

   typedef uint64_t u64_array[];
   
   
   function u64_array assemble();
      u64_array tmp;
      code_buf[code_pos++]  = (1<<35); // insert FIN instruction
      tmp                   = new [code_pos](code_buf);
      return tmp;
   endfunction // assemble
   
   
endclass // PFilterMicrocode
         
         




class CSimDrv_WR_Endpoint;

   protected CBusAccessor m_acc;
   protected uint64_t m_base;
   
   
   function new(CBusAccessor acc, uint64_t base);      m_acc   = acc;
      m_base  = base;
      
   endfunction // new

   task vlan_egress_untag(int vid, int untag);
      $display("VLAN_Write");
      m_acc.write(m_base + `ADDR_EP_VCR1, vid | ((untag ? 1: 0) << 12));
   endtask // vlan_egress_untag


   task pfilter_load_microcode(uint64_t mcode[]);
      int i;

      for(i=0;i<mcode.size();i++)
        begin
           m_acc.write(m_base + `ADDR_EP_PFCR1, (mcode[i] & 'hfff) << `EP_PFCR1_MM_DATA_LSB_OFFSET);
           
           m_acc.write(m_base + `ADDR_EP_PFCR0, 
                       (i << `EP_PFCR0_MM_ADDR_OFFSET) | 
                       (((mcode[i] >> 12) & 'hffffff) << `EP_PFCR0_MM_DATA_MSB_OFFSET) |
                       `EP_PFCR0_MM_WRITE);
        end
   endtask // pfilter_load_microcde

   task pfilter_enable(int enable);
      m_acc.write(m_base + `ADDR_EP_PFCR0, enable ? `EP_PFCR0_ENABLE: 0);
   endtask
        

endclass // CSimDrv_WR_Endpoint


module main;

   wire clk_sys, clk_ref_new, clk_ref_old;
   wire rst_n;

   wire [9:0] n2o, o2n;
   

   IWishboneMaster 
     #(
       .g_data_width(16),
       .g_addr_width(2))
   U_wrf_source
     (
      .clk_i(clk_sys),
      .rst_n_i(rst_n)
      );

   IWishboneSlave
     #(
       .g_data_width(16),
       .g_addr_width(2))
   U_wrf_sink
     (
      .clk_i(clk_sys),
      .rst_n_i(rst_n)
      );

   
   IWishboneMaster 
     #(
       .g_data_width(32),
       .g_addr_width(7))
   U_sys_bus_master
     (
      .clk_i(clk_sys),
      .rst_n_i(rst_n)
      );

   tbi_clock_rst_gen 

     #(
       .g_sysclk_period(8011))
   U_Clock_Gen 
       (
        .clk_ref_o (clk_ref_new),
        .clk_sys_o (clk_sys),
        .phy_rbclk_o (clk_ref_old),
        .rst_n_o(rst_n)
        );
   
   old_endpoint_test_wrapper
     U_oldep_wrap
       (
        .clk_sys_i(clk_sys),
        .clk_ref_i(clk_ref_old),
        .clk_rx_i(clk_ref_new),
        .rst_n_i(rst_n),
        .rd_i(n2o),
        .td_o(o2n)
        );


   wire[7:0] gtx_data;
   wire gtx_k;
   wire gtx_disparity;
   wire gtx_enc_error;
   wire [7:0] grx_data;
   wire grx_clk;
   wire grx_k;
   wire grx_enc_error;
   wire [3:0] grx_bitslide;
   wire gtp_rst;
   
   
   wr_tbi_phy U_Phy 
     (
      .serdes_rst_i (gtp_rst),
      .serdes_loopen_i(1'b0),
      .serdes_prbsen_i(1'b0),
      .serdes_enable_i(1'b1), 
      .serdes_syncen_i(1'b1),

      .serdes_tx_data_i  (gtx_data),
      .serdes_tx_k_i     (gtx_k),
      .serdes_tx_disparity_o (gtx_disparity),
      .serdes_tx_enc_err_o   (gtx_enc_error),

      .serdes_rx_data_o      (grx_data),
      .serdes_rx_k_o        (grx_k),
      .serdes_rx_enc_err_o  (grx_enc_error),
      .serdes_rx_bitslide_o (grx_bitslide),


      .tbi_refclk_i (clk_ref_new),
      .tbi_rbclk_i  (clk_ref_old),

      .tbi_td_o     (n2o),
      .tbi_rd_i     (o2n),
      .tbi_syncen_o (),
      .tbi_loopen_o (),
      .tbi_prbsen_o (),
      .tbi_enable_o ()
      );

   
   wr_endpoint
     #(
       .g_simulation          (1),
       .g_interface_mode      ("SERDES"),
       .g_rx_buffer_size_log2 (12),
       .g_with_timestamper    (1),
       .g_with_dmtd           (1),
       .g_with_dpi_classifier (1),
       .g_with_vlans          (1),
       .g_with_rtu            (1)
       ) DUT (
              .clk_ref_i (clk_ref_new),
              .clk_sys_i (clk_sys),
              .clk_dmtd_i (clk_ref_new),
              .rst_n_i  (rst_n),
              .pps_csync_p1_i (1'b0),

              .phy_rst_o   (),
              .phy_loopen_o (),
              .phy_prbsen_o (),
              .phy_enable_o (),
              .phy_syncen_o (),

              .phy_ref_clk_i(clk_ref_new),
              .phy_tx_data_o      (gtx_data),
              .phy_tx_k_o         (gtx_k),
              .phy_tx_disparity_i (gtx_disparity),
              .phy_tx_enc_err_i   (gtx_enc_error),

              .phy_rx_data_i     (grx_data),
              .phy_rx_clk_i      (clk_ref_old),
              .phy_rx_k_i        (grx_k),
              .phy_rx_enc_err_i  (grx_enc_error),
              .phy_rx_bitslide_i (4'b0),

              .src_dat_o   (U_wrf_sink.slave.dat_i),
              .src_adr_o   (U_wrf_sink.slave.adr),
              .src_sel_o   (U_wrf_sink.slave.sel),
              .src_cyc_o   (U_wrf_sink.slave.cyc),
              .src_stb_o   (U_wrf_sink.slave.stb),
              .src_we_o    (U_wrf_sink.slave.we),
              .src_stall_i (U_wrf_sink.slave.stall),
              .src_ack_i   (U_wrf_sink.slave.ack),

              .snk_dat_i   (U_wrf_source.master.dat_o[15:0]),
              .snk_adr_i   (U_wrf_source.master.adr[1:0]),
              .snk_sel_i   (U_wrf_source.master.sel[1:0]),
              .snk_cyc_i   (U_wrf_source.master.cyc),
              .snk_stb_i   (U_wrf_source.master.stb),
              .snk_we_i    (U_wrf_source.master.we),
              .snk_stall_o (U_wrf_source.master.stall),
              .snk_ack_o   (U_wrf_source.master.ack),
              .snk_err_o   (U_wrf_source.master.err),
              .snk_rty_o   (U_wrf_source.master.rty),

              .txtsu_port_id_o (),
              .txtsu_frame_id_o (),
              .txtsu_tsval_o (),
              .txtsu_valid_o (),
              .txtsu_ack_i (1'b1),

              .rtu_full_i (1'b0),
              .rtu_almost_full_i (1'b0),
              .rtu_rq_strobe_p1_o  (),
              .rtu_rq_smac_o  (),
              .rtu_rq_dmac_o (),
              .rtu_rq_vid_o  (),
              .rtu_rq_has_vid_o (),
              .rtu_rq_prio_o (),
              .rtu_rq_has_prio_o (),

              .wb_cyc_i(U_sys_bus_master.cyc),
              .wb_stb_i (U_sys_bus_master.stb),
              .wb_we_i (U_sys_bus_master.we),
              .wb_sel_i(U_sys_bus_master.sel),
              .wb_adr_i(U_sys_bus_master.adr[5:0]),
              .wb_dat_i(U_sys_bus_master.dat_o),
              .wb_dat_o(U_sys_bus_master.dat_i),
              .wb_ack_o (U_sys_bus_master.ack)
    );


   

   task automatic tx_test(int n_tries, int is_q, int unvid, EthPacketSource src, EthPacketSink sink);
      EthPacketGenerator gen = new;
      EthPacket pkt, tmpl, pkt2;
      EthPacket arr[];
      int i;

      
      arr            = new[n_tries](arr);
      
  
      tmpl           = new;
      tmpl.src       = '{1,2,3,4,5,6};
      tmpl.dst       = '{10,11,12,13,14,15};
      tmpl.has_smac  = 1;
      tmpl.is_q      = is_q;
      tmpl.vid       = 100;
      
      gen.set_randomization(EthPacketGenerator::SEQ_PAYLOAD | EthPacketGenerator::ETHERTYPE/* | EthPacketGenerator::TX_OOB*/) ;
      gen.set_template(tmpl);
      gen.set_size(64,1500);
      


    
      for(i=0;i<n_tries;i++)
           begin
              pkt  = gen.gen();
              $display("Tx %d", i);
              //   pkt.dump();
              src.send(pkt);
              arr[i]  = pkt;
           end

         for(i=0;i<n_tries;i++)
           begin
           sink.recv(pkt2);
              $display("rx %d", i);
              
           if(unvid)
             arr[i].is_q  = 0;
           
           if(!arr[i].equal(pkt2))
             begin
                $display("Fault at %d", i);
                
                arr[i].dump();
                pkt2.dump();
                $stop;
             end
           end // for (i=0;i<n_tries;i++)
      
   endtask // tx_test
   
   
     
   task test_tx_with_vlans(EthPacketSource src, EthPacketSink snk, CSimDrv_WR_Endpoint ep_drv);


   endtask // test_tx_with_vlans

   task init_pfilter(CSimDrv_WR_Endpoint ep_drv);
      PFilterMicrocode mc  = new;

      mc.cmp(0, 'h0a0b, 'hffff, PFilterMicrocode::MOV, 1);
      mc.cmp(1, 'h0c0a, 'hffff, PFilterMicrocode::AND, 1);
      mc.cmp(2, 'h0e0f, 'hffff, PFilterMicrocode::AND, 1);
      mc.logic2(2, 1, PFilterMicrocode::MOV, 0);
      mc.cmp(3, 'h0102, 'hffff, PFilterMicrocode::MOV, 1);
      mc.cmp(4, 'h030a, 'hffff, PFilterMicrocode::AND, 1);
      mc.cmp(5, 'h0506, 'hffff, PFilterMicrocode::AND, 1);
      mc.logic2(3, 1, PFilterMicrocode::MOV, 0);
      mc.cmp(6,'h86ba, 'hffff,  PFilterMicrocode::MOV, 4);

      mc.logic3(5, 2, PFilterMicrocode::AND, 3, PFilterMicrocode::OR, 4);
      
      ep_drv.pfilter_load_microcode(mc.assemble());
      ep_drv.pfilter_enable(1);
   endtask // init_pfilter
   

  
   
   initial begin
      CWishboneAccessor sys_bus;

      WBPacketSource src  = new(U_wrf_source.get_accessor());
      WBPacketSink sink   = new(U_wrf_sink.get_accessor());
      
      EthPacketSink o_sink;
      EthPacketSource o_src;
      
      CSimDrv_WR_Endpoint ep_drv;
      

      int i;
      

      

      @(posedge rst_n);
      @(posedge clk_sys);
      wait(!U_oldep_wrap.ready);
      


      sys_bus  = U_sys_bus_master.get_accessor();
      
      sys_bus.set_mode(CLASSIC);
      sys_bus.write(`ADDR_EP_ECR, `EP_ECR_TX_EN | `EP_ECR_RX_EN);
      sys_bus.write(`ADDR_EP_RFCR, 1518 << `EP_RFCR_MRU_OFFSET);
      

      ep_drv                                       = new(CBusAccessor'(sys_bus), 0);
      

      o_sink                                         = U_oldep_wrap.sink;
      o_src                                        = U_oldep_wrap.src;
      
      
 //     pkt.dump();
      U_wrf_source.settings.gen_random_throttling  = 1;
      U_wrf_source.settings.throttle_prob          = 0.02;
      
      ep_drv.vlan_egress_untag(100, 1);
/* -----\/----- EXCLUDED -----\/-----

      for(i=0;i<100;i++)
        begin
           $display("Iter: %d", i);
           tx_test(100, 0, 0, src, o_sink);
           tx_test(100, 1, 1, src, o_sink);
        end // initial begin
 -----/\----- EXCLUDED -----/\----- */

      #10000ns;

      init_pfilter(ep_drv);
      

      for(i=0;i<1;i++)
        begin
           $display("Iter: %d", i);
           tx_test(10, 0, 0, o_src, sink);
        end
   
                
                
   //   pkt2.dump();

      
      
   end // initial begin

   typedef struct {
      bit[31:0] crc;
      bit[15:0] data;
      bit half;
   } crc_tuple;


   crc_tuple orig[1000], dut[1000];
   int orig_pos     = 0, dut_pos = 0;
   
   
 /*always@(posedge DUT.U_Rx_Deframer.U_crc_size_checker.clk_sys_i)
     begin
        if(DUT.U_Rx_Deframer.U_crc_size_checker.src_fab_o.sof)
          orig_pos  = 0;

        if(DUT.U_Rx_Deframer.U_crc_size_checker.src_fab_o.dvalid) begin
           $display("d:%x %X", (orig_pos - 14) & 'hff, DUT.U_Rx_Deframer.U_crc_size_checker.src_fab_o.data[15:0]);
           orig_pos=orig_pos+2;
           
           end
     end*/ // UNMATCHED !!

   

/*    always@(posedge U_oldep_wrap.clk_sys_i)
      begin
         
        if(U_oldep_wrap.DUT.U_TX_FRA.U_tx_crc_generator.en_i) begin

           orig[orig_pos].data  =U_oldep_wrap.DUT.U_TX_FRA.U_tx_crc_generator.data_i;
           
           orig[orig_pos].crc   = U_oldep_wrap.DUT.U_TX_FRA.U_tx_crc_generator.crc_o;
           
           orig[orig_pos].half  = U_oldep_wrap.DUT.U_TX_FRA.U_tx_crc_generator.half_i;
           
           orig_pos++;
           
        end
     end // always@ (posedge U_oldep_wrap.clk_sys_i)

   initial begin
      int i;
      #10us;
      
      for(i=0;i<100;i++)
      $display("orig: %x %x dut: %x %x", orig[i].data, orig[i].crc, dut[i].data, dut[i].crc);
      
   end*/
  
   
endmodule // main




