--altdq_dqs CBX_SINGLE_OUTPUT_FILE="ON" DELAY_BUFFER_MODE="HIGH" DELAY_DQS_ENABLE_BY_HALF_CYCLE="TRUE" device_family="arriaii" DQ_HALF_RATE_USE_DATAOUTBYPASS="FALSE" DQ_INPUT_REG_ASYNC_MODE="NONE" DQ_INPUT_REG_CLK_SOURCE="INVERTED_DQS_BUS" DQ_INPUT_REG_MODE="DDIO" DQ_INPUT_REG_POWER_UP="LOW" DQ_INPUT_REG_SYNC_MODE="NONE" DQ_INPUT_REG_USE_CLKN="FALSE" DQ_IPA_ADD_INPUT_CYCLE_DELAY="FALSE" DQ_IPA_ADD_PHASE_TRANSFER_REG="FALSE" DQ_IPA_BYPASS_OUTPUT_REGISTER="FALSE" DQ_IPA_INVERT_PHASE="FALSE" DQ_IPA_PHASE_SETTING=0 DQ_OE_REG_ASYNC_MODE="NONE" DQ_OE_REG_MODE="FF" DQ_OE_REG_POWER_UP="LOW" DQ_OE_REG_SYNC_MODE="NONE" DQ_OUTPUT_REG_ASYNC_MODE="CLEAR" DQ_OUTPUT_REG_MODE="DDIO" DQ_OUTPUT_REG_POWER_UP="LOW" DQ_OUTPUT_REG_SYNC_MODE="NONE" DQS_CTRL_LATCHES_ENABLE="FALSE" DQS_DELAY_CHAIN_DELAYCTRLIN_SOURCE="DLL" DQS_DELAY_CHAIN_PHASE_SETTING=2 DQS_DQSN_MODE="DIFFERENTIAL" DQS_ENABLE_CTRL_ADD_PHASE_TRANSFER_REG="FALSE" DQS_ENABLE_CTRL_INVERT_PHASE="FALSE" DQS_ENABLE_CTRL_PHASE_SETTING=0 DQS_INPUT_FREQUENCY="400.0 MHz" DQS_OE_REG_ASYNC_MODE="NONE" DQS_OE_REG_MODE="FF" DQS_OE_REG_POWER_UP="LOW" DQS_OE_REG_SYNC_MODE="NONE" DQS_OFFSETCTRL_ENABLE="FALSE" DQS_OUTPUT_REG_ASYNC_MODE="NONE" DQS_OUTPUT_REG_MODE="DDIO" DQS_OUTPUT_REG_POWER_UP="LOW" DQS_OUTPUT_REG_SYNC_MODE="NONE" DQS_PHASE_SHIFT=9000 IO_CLOCK_DIVIDER_CLK_SOURCE="CORE" IO_CLOCK_DIVIDER_INVERT_PHASE="FALSE" IO_CLOCK_DIVIDER_PHASE_SETTING=0 LEVEL_DQS_ENABLE="FALSE" NUMBER_OF_BIDIR_DQ=8 NUMBER_OF_CLK_DIVIDER=0 NUMBER_OF_INPUT_DQ=0 NUMBER_OF_OUTPUT_DQ=1 OCT_REG_MODE="NONE" USE_DQ_INPUT_DELAY_CHAIN="FALSE" USE_DQ_IPA="FALSE" USE_DQ_IPA_PHASECTRLIN="FALSE" USE_DQ_OE_DELAY_CHAIN1="FALSE" USE_DQ_OE_DELAY_CHAIN2="FALSE" USE_DQ_OE_PATH="TRUE" USE_DQ_OUTPUT_DELAY_CHAIN1="FALSE" USE_DQ_OUTPUT_DELAY_CHAIN2="FALSE" USE_DQS="TRUE" USE_DQS_DELAY_CHAIN="TRUE" USE_DQS_DELAY_CHAIN_PHASECTRLIN="FALSE" USE_DQS_ENABLE="TRUE" USE_DQS_ENABLE_CTRL="TRUE" USE_DQS_ENABLE_CTRL_PHASECTRLIN="FALSE" USE_DQS_INPUT_DELAY_CHAIN="FALSE" USE_DQS_INPUT_PATH="TRUE" USE_DQS_OE_DELAY_CHAIN1="FALSE" USE_DQS_OE_DELAY_CHAIN2="FALSE" USE_DQS_OE_PATH="TRUE" USE_DQS_OUTPUT_DELAY_CHAIN1="FALSE" USE_DQS_OUTPUT_DELAY_CHAIN2="FALSE" USE_DQS_OUTPUT_PATH="TRUE" USE_DQSBUSOUT_DELAY_CHAIN="FALSE" USE_DQSENABLE_DELAY_CHAIN="FALSE" USE_DYNAMIC_OCT="FALSE" USE_HALF_RATE="FALSE" USE_IO_CLOCK_DIVIDER_MASTERIN="FALSE" USE_IO_CLOCK_DIVIDER_PHASECTRLIN="FALSE" USE_OCT_DELAY_CHAIN1="FALSE" USE_OCT_DELAY_CHAIN2="FALSE" bidir_dq_areset bidir_dq_input_data_in bidir_dq_input_data_out_high bidir_dq_input_data_out_low bidir_dq_oe_in bidir_dq_oe_out bidir_dq_output_data_in_high bidir_dq_output_data_in_low bidir_dq_output_data_out dll_delayctrlin dq_input_reg_clk dq_output_reg_clk dqs_enable_ctrl_clk dqs_enable_ctrl_in dqs_input_data_in dqs_oe_in dqs_oe_out dqs_output_data_in_high dqs_output_data_in_low dqs_output_data_out dqs_output_reg_clk dqsn_oe_in dqsn_oe_out output_dq_oe_in output_dq_oe_out output_dq_output_data_in_high output_dq_output_data_in_low output_dq_output_data_out
--VERSION_BEGIN 11.1SP2 cbx_altdq_dqs 2012:01:25:21:12:11:SJ cbx_mgl 2012:01:25:21:26:09:SJ cbx_stratixiii 2012:01:25:21:12:11:SJ  VERSION_END


-- Copyright (C) 1991-2011 Altera Corporation
--  Your use of Altera Corporation's design tools, logic functions 
--  and other software and tools, and its AMPP partner logic 
--  functions, and any output files from any of the foregoing 
--  (including device programming or simulation files), and any 
--  associated documentation or information are expressly subject 
--  to the terms and conditions of the Altera Program License 
--  Subscription Agreement, Altera MegaCore Function License 
--  Agreement, or other applicable license agreement, including, 
--  without limitation, that your use is for the sole purpose of 
--  programming logic devices manufactured by Altera and sold by 
--  Altera or its authorized distributors.  Please refer to the 
--  applicable agreement for further details.



 LIBRARY altera;
 USE altera.all;

 LIBRARY arriaii;
 USE arriaii.all;

--synthesis_resources = arriaii_ddio_in 8 arriaii_ddio_out 10 arriaii_dqs_delay_chain 1 arriaii_dqs_enable 1 arriaii_dqs_enable_ctrl 1 reg 11 
 LIBRARY ieee;
 USE ieee.std_logic_1164.all;

 ENTITY  ddr3_mem_phy_alt_mem_phy_dq_dqs IS 
	 PORT 
	 ( 
		 bidir_dq_areset	:	IN  STD_LOGIC_VECTOR (7 DOWNTO 0) := (OTHERS => '0');
		 bidir_dq_input_data_in	:	IN  STD_LOGIC_VECTOR (7 DOWNTO 0) := (OTHERS => '0');
		 bidir_dq_input_data_out_high	:	OUT  STD_LOGIC_VECTOR (7 DOWNTO 0);
		 bidir_dq_input_data_out_low	:	OUT  STD_LOGIC_VECTOR (7 DOWNTO 0);
		 bidir_dq_oe_in	:	IN  STD_LOGIC_VECTOR (7 DOWNTO 0) := (OTHERS => '0');
		 bidir_dq_oe_out	:	OUT  STD_LOGIC_VECTOR (7 DOWNTO 0);
		 bidir_dq_output_data_in_high	:	IN  STD_LOGIC_VECTOR (7 DOWNTO 0) := (OTHERS => '0');
		 bidir_dq_output_data_in_low	:	IN  STD_LOGIC_VECTOR (7 DOWNTO 0) := (OTHERS => '0');
		 bidir_dq_output_data_out	:	OUT  STD_LOGIC_VECTOR (7 DOWNTO 0);
		 dll_delayctrlin	:	IN  STD_LOGIC_VECTOR (5 DOWNTO 0) := (OTHERS => '0');
		 dq_input_reg_clk	:	IN  STD_LOGIC := '0';
		 dq_output_reg_clk	:	IN  STD_LOGIC := '0';
		 dqs_enable_ctrl_clk	:	IN  STD_LOGIC := '1';
		 dqs_enable_ctrl_in	:	IN  STD_LOGIC := '1';
		 dqs_input_data_in	:	IN  STD_LOGIC_VECTOR (0 DOWNTO 0) := (OTHERS => '0');
		 dqs_oe_in	:	IN  STD_LOGIC_VECTOR (0 DOWNTO 0) := (OTHERS => '0');
		 dqs_oe_out	:	OUT  STD_LOGIC_VECTOR (0 DOWNTO 0);
		 dqs_output_data_in_high	:	IN  STD_LOGIC_VECTOR (0 DOWNTO 0) := (OTHERS => '0');
		 dqs_output_data_in_low	:	IN  STD_LOGIC_VECTOR (0 DOWNTO 0) := (OTHERS => '0');
		 dqs_output_data_out	:	OUT  STD_LOGIC_VECTOR (0 DOWNTO 0);
		 dqs_output_reg_clk	:	IN  STD_LOGIC := '0';
		 dqsn_oe_in	:	IN  STD_LOGIC_VECTOR (0 DOWNTO 0) := (OTHERS => '0');
		 dqsn_oe_out	:	OUT  STD_LOGIC_VECTOR (0 DOWNTO 0);
		 output_dq_oe_in	:	IN  STD_LOGIC_VECTOR (0 DOWNTO 0) := (OTHERS => '0');
		 output_dq_oe_out	:	OUT  STD_LOGIC_VECTOR (0 DOWNTO 0);
		 output_dq_output_data_in_high	:	IN  STD_LOGIC_VECTOR (0 DOWNTO 0) := (OTHERS => '0');
		 output_dq_output_data_in_low	:	IN  STD_LOGIC_VECTOR (0 DOWNTO 0) := (OTHERS => '0');
		 output_dq_output_data_out	:	OUT  STD_LOGIC_VECTOR (0 DOWNTO 0)
	 ); 
 END ddr3_mem_phy_alt_mem_phy_dq_dqs;

 ARCHITECTURE RTL OF ddr3_mem_phy_alt_mem_phy_dq_dqs IS

	 ATTRIBUTE synthesis_clearbox : natural;
	 ATTRIBUTE synthesis_clearbox OF RTL : ARCHITECTURE IS 1;
	 ATTRIBUTE ALTERA_ATTRIBUTE : string;
	 ATTRIBUTE ALTERA_ATTRIBUTE OF RTL : ARCHITECTURE IS "-name DQ_GROUP 9 -from dqs_0_delay_chain_inst -to bidir_dq_0_output_ddio_out_inst;-name DQ_GROUP 9 -from dqs_0_delay_chain_inst -to bidir_dq_1_output_ddio_out_inst;-name DQ_GROUP 9 -from dqs_0_delay_chain_inst -to bidir_dq_2_output_ddio_out_inst;-name DQ_GROUP 9 -from dqs_0_delay_chain_inst -to bidir_dq_3_output_ddio_out_inst;-name DQ_GROUP 9 -from dqs_0_delay_chain_inst -to bidir_dq_4_output_ddio_out_inst;-name DQ_GROUP 9 -from dqs_0_delay_chain_inst -to bidir_dq_5_output_ddio_out_inst;-name DQ_GROUP 9 -from dqs_0_delay_chain_inst -to bidir_dq_6_output_ddio_out_inst;-name DQ_GROUP 9 -from dqs_0_delay_chain_inst -to bidir_dq_7_output_ddio_out_inst;-name DQ_GROUP 9 -from dqs_0_delay_chain_inst -to output_dq_0_output_ddio_out_inst";

	 SIGNAL	 bidir_dq_0_oe_ff_inst	:	STD_LOGIC
	 -- synopsys translate_off
	  := '0'
	 -- synopsys translate_on
	 ;
	 ATTRIBUTE ALTERA_ATTRIBUTE OF bidir_dq_0_oe_ff_inst : SIGNAL IS "FAST_OUTPUT_ENABLE_REGISTER=ON";

	 SIGNAL  wire_bidir_dq_0_oe_ff_inst_w_lg_q25w	:	STD_LOGIC_VECTOR (0 DOWNTO 0);
	 SIGNAL	 bidir_dq_1_oe_ff_inst	:	STD_LOGIC
	 -- synopsys translate_off
	  := '0'
	 -- synopsys translate_on
	 ;
	 ATTRIBUTE ALTERA_ATTRIBUTE OF bidir_dq_1_oe_ff_inst : SIGNAL IS "FAST_OUTPUT_ENABLE_REGISTER=ON";

	 SIGNAL  wire_bidir_dq_1_oe_ff_inst_w_lg_q40w	:	STD_LOGIC_VECTOR (0 DOWNTO 0);
	 SIGNAL	 bidir_dq_2_oe_ff_inst	:	STD_LOGIC
	 -- synopsys translate_off
	  := '0'
	 -- synopsys translate_on
	 ;
	 ATTRIBUTE ALTERA_ATTRIBUTE OF bidir_dq_2_oe_ff_inst : SIGNAL IS "FAST_OUTPUT_ENABLE_REGISTER=ON";

	 SIGNAL  wire_bidir_dq_2_oe_ff_inst_w_lg_q51w	:	STD_LOGIC_VECTOR (0 DOWNTO 0);
	 SIGNAL	 bidir_dq_3_oe_ff_inst	:	STD_LOGIC
	 -- synopsys translate_off
	  := '0'
	 -- synopsys translate_on
	 ;
	 ATTRIBUTE ALTERA_ATTRIBUTE OF bidir_dq_3_oe_ff_inst : SIGNAL IS "FAST_OUTPUT_ENABLE_REGISTER=ON";

	 SIGNAL  wire_bidir_dq_3_oe_ff_inst_w_lg_q62w	:	STD_LOGIC_VECTOR (0 DOWNTO 0);
	 SIGNAL	 bidir_dq_4_oe_ff_inst	:	STD_LOGIC
	 -- synopsys translate_off
	  := '0'
	 -- synopsys translate_on
	 ;
	 ATTRIBUTE ALTERA_ATTRIBUTE OF bidir_dq_4_oe_ff_inst : SIGNAL IS "FAST_OUTPUT_ENABLE_REGISTER=ON";

	 SIGNAL  wire_bidir_dq_4_oe_ff_inst_w_lg_q73w	:	STD_LOGIC_VECTOR (0 DOWNTO 0);
	 SIGNAL	 bidir_dq_5_oe_ff_inst	:	STD_LOGIC
	 -- synopsys translate_off
	  := '0'
	 -- synopsys translate_on
	 ;
	 ATTRIBUTE ALTERA_ATTRIBUTE OF bidir_dq_5_oe_ff_inst : SIGNAL IS "FAST_OUTPUT_ENABLE_REGISTER=ON";

	 SIGNAL  wire_bidir_dq_5_oe_ff_inst_w_lg_q84w	:	STD_LOGIC_VECTOR (0 DOWNTO 0);
	 SIGNAL	 bidir_dq_6_oe_ff_inst	:	STD_LOGIC
	 -- synopsys translate_off
	  := '0'
	 -- synopsys translate_on
	 ;
	 ATTRIBUTE ALTERA_ATTRIBUTE OF bidir_dq_6_oe_ff_inst : SIGNAL IS "FAST_OUTPUT_ENABLE_REGISTER=ON";

	 SIGNAL  wire_bidir_dq_6_oe_ff_inst_w_lg_q95w	:	STD_LOGIC_VECTOR (0 DOWNTO 0);
	 SIGNAL	 bidir_dq_7_oe_ff_inst	:	STD_LOGIC
	 -- synopsys translate_off
	  := '0'
	 -- synopsys translate_on
	 ;
	 ATTRIBUTE ALTERA_ATTRIBUTE OF bidir_dq_7_oe_ff_inst : SIGNAL IS "FAST_OUTPUT_ENABLE_REGISTER=ON";

	 SIGNAL  wire_bidir_dq_7_oe_ff_inst_w_lg_q106w	:	STD_LOGIC_VECTOR (0 DOWNTO 0);
	 SIGNAL	 dqs_0_oe_ff_inst	:	STD_LOGIC
	 -- synopsys translate_off
	  := '0'
	 -- synopsys translate_on
	 ;
	 ATTRIBUTE ALTERA_ATTRIBUTE OF dqs_0_oe_ff_inst : SIGNAL IS "FAST_OUTPUT_ENABLE_REGISTER=ON";

	 SIGNAL  wire_dqs_0_oe_ff_inst_w_lg_q10w	:	STD_LOGIC_VECTOR (0 DOWNTO 0);
	 SIGNAL	 dqsn_0_oe_ff_inst	:	STD_LOGIC
	 -- synopsys translate_off
	  := '0'
	 -- synopsys translate_on
	 ;
	 ATTRIBUTE ALTERA_ATTRIBUTE OF dqsn_0_oe_ff_inst : SIGNAL IS "FAST_OUTPUT_ENABLE_REGISTER=ON";

	 SIGNAL  wire_dqsn_0_oe_ff_inst_w_lg_q15w	:	STD_LOGIC_VECTOR (0 DOWNTO 0);
	 SIGNAL	 output_dq_0_oe_ff_inst	:	STD_LOGIC
	 -- synopsys translate_off
	  := '0'
	 -- synopsys translate_on
	 ;
	 ATTRIBUTE ALTERA_ATTRIBUTE OF output_dq_0_oe_ff_inst : SIGNAL IS "FAST_OUTPUT_ENABLE_REGISTER=ON";

	 SIGNAL  wire_output_dq_0_oe_ff_inst_w_lg_q117w	:	STD_LOGIC_VECTOR (0 DOWNTO 0);
	 SIGNAL  wire_bidir_dq_0_ddio_in_inst_clk	:	STD_LOGIC;
	 SIGNAL  wire_w_lg_w_dqs_bus_wire_range2w29w	:	STD_LOGIC_VECTOR (0 DOWNTO 0);
	 SIGNAL  wire_bidir_dq_0_ddio_in_inst_regouthi	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_0_ddio_in_inst_regoutlo	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_1_ddio_in_inst_clk	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_1_ddio_in_inst_regouthi	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_1_ddio_in_inst_regoutlo	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_2_ddio_in_inst_clk	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_2_ddio_in_inst_regouthi	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_2_ddio_in_inst_regoutlo	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_3_ddio_in_inst_clk	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_3_ddio_in_inst_regouthi	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_3_ddio_in_inst_regoutlo	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_4_ddio_in_inst_clk	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_4_ddio_in_inst_regouthi	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_4_ddio_in_inst_regoutlo	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_5_ddio_in_inst_clk	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_5_ddio_in_inst_regouthi	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_5_ddio_in_inst_regoutlo	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_6_ddio_in_inst_clk	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_6_ddio_in_inst_regouthi	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_6_ddio_in_inst_regoutlo	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_7_ddio_in_inst_clk	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_7_ddio_in_inst_regouthi	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_7_ddio_in_inst_regoutlo	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_0_output_ddio_out_inst_dataout	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_1_output_ddio_out_inst_dataout	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_2_output_ddio_out_inst_dataout	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_3_output_ddio_out_inst_dataout	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_4_output_ddio_out_inst_dataout	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_5_output_ddio_out_inst_dataout	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_6_output_ddio_out_inst_dataout	:	STD_LOGIC;
	 SIGNAL  wire_bidir_dq_7_output_ddio_out_inst_dataout	:	STD_LOGIC;
	 SIGNAL  wire_dqs_0_output_ddio_out_inst_dataout	:	STD_LOGIC;
	 SIGNAL  wire_output_dq_0_output_ddio_out_inst_dataout	:	STD_LOGIC;
	 SIGNAL  wire_dqs_0_delay_chain_inst_dqsbusout	:	STD_LOGIC;
	 SIGNAL  wire_dqs_0_enable_inst_dqsbusout	:	STD_LOGIC;
	 SIGNAL  wire_dqs_0_enable_ctrl_inst_dqsenableout	:	STD_LOGIC;
	 SIGNAL  dqs_bus_wire :	STD_LOGIC_VECTOR (0 DOWNTO 0);
	 COMPONENT  arriaii_ddio_in
	 GENERIC 
	 (
		async_mode	:	STRING := "none";
		power_up	:	STRING := "low";
		sync_mode	:	STRING := "none";
		use_clkn	:	STRING := "false";
		lpm_type	:	STRING := "arriaii_ddio_in"
	 );
	 PORT
	 ( 
		areset	:	IN STD_LOGIC := '0';
		clk	:	IN STD_LOGIC := '0';
		clkn	:	IN STD_LOGIC := '0';
		datain	:	IN STD_LOGIC := '0';
		ena	:	IN STD_LOGIC := '1';
		regouthi	:	OUT STD_LOGIC;
		regoutlo	:	OUT STD_LOGIC;
		sreset	:	IN STD_LOGIC := '0'
	 ); 
	 END COMPONENT;
	 COMPONENT  arriaii_ddio_out
	 GENERIC 
	 (
		async_mode	:	STRING := "none";
		half_rate_mode	:	STRING := "false";
		power_up	:	STRING := "low";
		sync_mode	:	STRING := "none";
		use_new_clocking_model	:	STRING := "false";
		lpm_type	:	STRING := "arriaii_ddio_out"
	 );
	 PORT
	 ( 
		areset	:	IN STD_LOGIC := '0';
		clk	:	IN STD_LOGIC := '0';
		clkhi	:	IN STD_LOGIC := '0';
		clklo	:	IN STD_LOGIC := '0';
		datainhi	:	IN STD_LOGIC := '0';
		datainlo	:	IN STD_LOGIC := '0';
		dataout	:	OUT STD_LOGIC;
		ena	:	IN STD_LOGIC := '1';
		muxsel	:	IN STD_LOGIC := '0';
		sreset	:	IN STD_LOGIC := '0'
	 ); 
	 END COMPONENT;
	 COMPONENT  arriaii_dqs_delay_chain
	 GENERIC 
	 (
		delay_buffer_mode	:	STRING := "low";
		dqs_ctrl_latches_enable	:	STRING := "false";
		dqs_input_frequency	:	STRING := "UNUSED";
		dqs_offsetctrl_enable	:	STRING := "false";
		dqs_phase_shift	:	NATURAL := 0;
		phase_setting	:	NATURAL := 0;
		sim_buffer_delay_increment	:	NATURAL := 10;
		sim_high_buffer_intrinsic_delay	:	NATURAL := 175;
		sim_low_buffer_intrinsic_delay	:	NATURAL := 350;
		test_enable	:	STRING := "false";
		test_select	:	NATURAL := 0;
		lpm_type	:	STRING := "arriaii_dqs_delay_chain"
	 );
	 PORT
	 ( 
		delayctrlin	:	IN STD_LOGIC_VECTOR(5 DOWNTO 0) := (OTHERS => '0');
		dqsbusout	:	OUT STD_LOGIC;
		dqsin	:	IN STD_LOGIC := '0';
		dqsupdateen	:	IN STD_LOGIC := '0';
		offsetctrlin	:	IN STD_LOGIC_VECTOR(5 DOWNTO 0) := (OTHERS => '0')
	 ); 
	 END COMPONENT;
	 COMPONENT  arriaii_dqs_enable
	 PORT
	 ( 
		dqsbusout	:	OUT STD_LOGIC;
		dqsenable	:	IN STD_LOGIC := '1';
		dqsin	:	IN STD_LOGIC := '0'
	 ); 
	 END COMPONENT;
	 COMPONENT  arriaii_dqs_enable_ctrl
	 GENERIC 
	 (
		delay_dqs_enable_by_half_cycle	:	STRING := "false";
		lpm_type	:	STRING := "arriaii_dqs_enable_ctrl"
	 );
	 PORT
	 ( 
		clk	:	IN STD_LOGIC := '1';
		dqsenablein	:	IN STD_LOGIC := '1';
		dqsenableout	:	OUT STD_LOGIC
	 ); 
	 END COMPONENT;
 BEGIN

	bidir_dq_input_data_out_high <= ( wire_bidir_dq_7_ddio_in_inst_regouthi & wire_bidir_dq_6_ddio_in_inst_regouthi & wire_bidir_dq_5_ddio_in_inst_regouthi & wire_bidir_dq_4_ddio_in_inst_regouthi & wire_bidir_dq_3_ddio_in_inst_regouthi & wire_bidir_dq_2_ddio_in_inst_regouthi & wire_bidir_dq_1_ddio_in_inst_regouthi & wire_bidir_dq_0_ddio_in_inst_regouthi);
	bidir_dq_input_data_out_low <= ( wire_bidir_dq_7_ddio_in_inst_regoutlo & wire_bidir_dq_6_ddio_in_inst_regoutlo & wire_bidir_dq_5_ddio_in_inst_regoutlo & wire_bidir_dq_4_ddio_in_inst_regoutlo & wire_bidir_dq_3_ddio_in_inst_regoutlo & wire_bidir_dq_2_ddio_in_inst_regoutlo & wire_bidir_dq_1_ddio_in_inst_regoutlo & wire_bidir_dq_0_ddio_in_inst_regoutlo);
	bidir_dq_oe_out <= ( wire_bidir_dq_7_oe_ff_inst_w_lg_q106w & wire_bidir_dq_6_oe_ff_inst_w_lg_q95w & wire_bidir_dq_5_oe_ff_inst_w_lg_q84w & wire_bidir_dq_4_oe_ff_inst_w_lg_q73w & wire_bidir_dq_3_oe_ff_inst_w_lg_q62w & wire_bidir_dq_2_oe_ff_inst_w_lg_q51w & wire_bidir_dq_1_oe_ff_inst_w_lg_q40w & wire_bidir_dq_0_oe_ff_inst_w_lg_q25w);
	bidir_dq_output_data_out <= ( wire_bidir_dq_7_output_ddio_out_inst_dataout & wire_bidir_dq_6_output_ddio_out_inst_dataout & wire_bidir_dq_5_output_ddio_out_inst_dataout & wire_bidir_dq_4_output_ddio_out_inst_dataout & wire_bidir_dq_3_output_ddio_out_inst_dataout & wire_bidir_dq_2_output_ddio_out_inst_dataout & wire_bidir_dq_1_output_ddio_out_inst_dataout & wire_bidir_dq_0_output_ddio_out_inst_dataout);
	dqs_bus_wire(0) <= ( wire_dqs_0_enable_inst_dqsbusout);
	dqs_oe_out <= ( wire_dqs_0_oe_ff_inst_w_lg_q10w);
	dqs_output_data_out(0) <= ( wire_dqs_0_output_ddio_out_inst_dataout);
	dqsn_oe_out <= ( wire_dqsn_0_oe_ff_inst_w_lg_q15w);
	output_dq_oe_out <= ( wire_output_dq_0_oe_ff_inst_w_lg_q117w);
	output_dq_output_data_out(0) <= ( wire_output_dq_0_output_ddio_out_inst_dataout);
	PROCESS (dq_output_reg_clk)
	BEGIN
		IF (dq_output_reg_clk = '1' AND dq_output_reg_clk'event) THEN bidir_dq_0_oe_ff_inst <= (NOT bidir_dq_oe_in(0));
		END IF;
	END PROCESS;
	wire_bidir_dq_0_oe_ff_inst_w_lg_q25w(0) <= NOT bidir_dq_0_oe_ff_inst;
	PROCESS (dq_output_reg_clk)
	BEGIN
		IF (dq_output_reg_clk = '1' AND dq_output_reg_clk'event) THEN bidir_dq_1_oe_ff_inst <= (NOT bidir_dq_oe_in(1));
		END IF;
	END PROCESS;
	wire_bidir_dq_1_oe_ff_inst_w_lg_q40w(0) <= NOT bidir_dq_1_oe_ff_inst;
	PROCESS (dq_output_reg_clk)
	BEGIN
		IF (dq_output_reg_clk = '1' AND dq_output_reg_clk'event) THEN bidir_dq_2_oe_ff_inst <= (NOT bidir_dq_oe_in(2));
		END IF;
	END PROCESS;
	wire_bidir_dq_2_oe_ff_inst_w_lg_q51w(0) <= NOT bidir_dq_2_oe_ff_inst;
	PROCESS (dq_output_reg_clk)
	BEGIN
		IF (dq_output_reg_clk = '1' AND dq_output_reg_clk'event) THEN bidir_dq_3_oe_ff_inst <= (NOT bidir_dq_oe_in(3));
		END IF;
	END PROCESS;
	wire_bidir_dq_3_oe_ff_inst_w_lg_q62w(0) <= NOT bidir_dq_3_oe_ff_inst;
	PROCESS (dq_output_reg_clk)
	BEGIN
		IF (dq_output_reg_clk = '1' AND dq_output_reg_clk'event) THEN bidir_dq_4_oe_ff_inst <= (NOT bidir_dq_oe_in(4));
		END IF;
	END PROCESS;
	wire_bidir_dq_4_oe_ff_inst_w_lg_q73w(0) <= NOT bidir_dq_4_oe_ff_inst;
	PROCESS (dq_output_reg_clk)
	BEGIN
		IF (dq_output_reg_clk = '1' AND dq_output_reg_clk'event) THEN bidir_dq_5_oe_ff_inst <= (NOT bidir_dq_oe_in(5));
		END IF;
	END PROCESS;
	wire_bidir_dq_5_oe_ff_inst_w_lg_q84w(0) <= NOT bidir_dq_5_oe_ff_inst;
	PROCESS (dq_output_reg_clk)
	BEGIN
		IF (dq_output_reg_clk = '1' AND dq_output_reg_clk'event) THEN bidir_dq_6_oe_ff_inst <= (NOT bidir_dq_oe_in(6));
		END IF;
	END PROCESS;
	wire_bidir_dq_6_oe_ff_inst_w_lg_q95w(0) <= NOT bidir_dq_6_oe_ff_inst;
	PROCESS (dq_output_reg_clk)
	BEGIN
		IF (dq_output_reg_clk = '1' AND dq_output_reg_clk'event) THEN bidir_dq_7_oe_ff_inst <= (NOT bidir_dq_oe_in(7));
		END IF;
	END PROCESS;
	wire_bidir_dq_7_oe_ff_inst_w_lg_q106w(0) <= NOT bidir_dq_7_oe_ff_inst;
	PROCESS (dqs_output_reg_clk)
	BEGIN
		IF (dqs_output_reg_clk = '1' AND dqs_output_reg_clk'event) THEN dqs_0_oe_ff_inst <= (NOT dqs_oe_in(0));
		END IF;
	END PROCESS;
	wire_dqs_0_oe_ff_inst_w_lg_q10w(0) <= NOT dqs_0_oe_ff_inst;
	PROCESS (dqs_output_reg_clk)
	BEGIN
		IF (dqs_output_reg_clk = '1' AND dqs_output_reg_clk'event) THEN dqsn_0_oe_ff_inst <= (NOT dqsn_oe_in(0));
		END IF;
	END PROCESS;
	wire_dqsn_0_oe_ff_inst_w_lg_q15w(0) <= NOT dqsn_0_oe_ff_inst;
	PROCESS (dq_output_reg_clk)
	BEGIN
		IF (dq_output_reg_clk = '1' AND dq_output_reg_clk'event) THEN output_dq_0_oe_ff_inst <= (NOT output_dq_oe_in(0));
		END IF;
	END PROCESS;
	wire_output_dq_0_oe_ff_inst_w_lg_q117w(0) <= NOT output_dq_0_oe_ff_inst;
	wire_bidir_dq_0_ddio_in_inst_clk <= wire_w_lg_w_dqs_bus_wire_range2w29w(0);
	wire_w_lg_w_dqs_bus_wire_range2w29w(0) <= NOT dqs_bus_wire(0);
	bidir_dq_0_ddio_in_inst :  arriaii_ddio_in
	  GENERIC MAP (
		async_mode => "none",
		sync_mode => "none",
		use_clkn => "false"
	  )
	  PORT MAP ( 
		clk => wire_bidir_dq_0_ddio_in_inst_clk,
		datain => bidir_dq_input_data_in(0),
		regouthi => wire_bidir_dq_0_ddio_in_inst_regouthi,
		regoutlo => wire_bidir_dq_0_ddio_in_inst_regoutlo
	  );
	wire_bidir_dq_1_ddio_in_inst_clk <= wire_w_lg_w_dqs_bus_wire_range2w29w(0);
	bidir_dq_1_ddio_in_inst :  arriaii_ddio_in
	  GENERIC MAP (
		async_mode => "none",
		sync_mode => "none",
		use_clkn => "false"
	  )
	  PORT MAP ( 
		clk => wire_bidir_dq_1_ddio_in_inst_clk,
		datain => bidir_dq_input_data_in(1),
		regouthi => wire_bidir_dq_1_ddio_in_inst_regouthi,
		regoutlo => wire_bidir_dq_1_ddio_in_inst_regoutlo
	  );
	wire_bidir_dq_2_ddio_in_inst_clk <= wire_w_lg_w_dqs_bus_wire_range2w29w(0);
	bidir_dq_2_ddio_in_inst :  arriaii_ddio_in
	  GENERIC MAP (
		async_mode => "none",
		sync_mode => "none",
		use_clkn => "false"
	  )
	  PORT MAP ( 
		clk => wire_bidir_dq_2_ddio_in_inst_clk,
		datain => bidir_dq_input_data_in(2),
		regouthi => wire_bidir_dq_2_ddio_in_inst_regouthi,
		regoutlo => wire_bidir_dq_2_ddio_in_inst_regoutlo
	  );
	wire_bidir_dq_3_ddio_in_inst_clk <= wire_w_lg_w_dqs_bus_wire_range2w29w(0);
	bidir_dq_3_ddio_in_inst :  arriaii_ddio_in
	  GENERIC MAP (
		async_mode => "none",
		sync_mode => "none",
		use_clkn => "false"
	  )
	  PORT MAP ( 
		clk => wire_bidir_dq_3_ddio_in_inst_clk,
		datain => bidir_dq_input_data_in(3),
		regouthi => wire_bidir_dq_3_ddio_in_inst_regouthi,
		regoutlo => wire_bidir_dq_3_ddio_in_inst_regoutlo
	  );
	wire_bidir_dq_4_ddio_in_inst_clk <= wire_w_lg_w_dqs_bus_wire_range2w29w(0);
	bidir_dq_4_ddio_in_inst :  arriaii_ddio_in
	  GENERIC MAP (
		async_mode => "none",
		sync_mode => "none",
		use_clkn => "false"
	  )
	  PORT MAP ( 
		clk => wire_bidir_dq_4_ddio_in_inst_clk,
		datain => bidir_dq_input_data_in(4),
		regouthi => wire_bidir_dq_4_ddio_in_inst_regouthi,
		regoutlo => wire_bidir_dq_4_ddio_in_inst_regoutlo
	  );
	wire_bidir_dq_5_ddio_in_inst_clk <= wire_w_lg_w_dqs_bus_wire_range2w29w(0);
	bidir_dq_5_ddio_in_inst :  arriaii_ddio_in
	  GENERIC MAP (
		async_mode => "none",
		sync_mode => "none",
		use_clkn => "false"
	  )
	  PORT MAP ( 
		clk => wire_bidir_dq_5_ddio_in_inst_clk,
		datain => bidir_dq_input_data_in(5),
		regouthi => wire_bidir_dq_5_ddio_in_inst_regouthi,
		regoutlo => wire_bidir_dq_5_ddio_in_inst_regoutlo
	  );
	wire_bidir_dq_6_ddio_in_inst_clk <= wire_w_lg_w_dqs_bus_wire_range2w29w(0);
	bidir_dq_6_ddio_in_inst :  arriaii_ddio_in
	  GENERIC MAP (
		async_mode => "none",
		sync_mode => "none",
		use_clkn => "false"
	  )
	  PORT MAP ( 
		clk => wire_bidir_dq_6_ddio_in_inst_clk,
		datain => bidir_dq_input_data_in(6),
		regouthi => wire_bidir_dq_6_ddio_in_inst_regouthi,
		regoutlo => wire_bidir_dq_6_ddio_in_inst_regoutlo
	  );
	wire_bidir_dq_7_ddio_in_inst_clk <= wire_w_lg_w_dqs_bus_wire_range2w29w(0);
	bidir_dq_7_ddio_in_inst :  arriaii_ddio_in
	  GENERIC MAP (
		async_mode => "none",
		sync_mode => "none",
		use_clkn => "false"
	  )
	  PORT MAP ( 
		clk => wire_bidir_dq_7_ddio_in_inst_clk,
		datain => bidir_dq_input_data_in(7),
		regouthi => wire_bidir_dq_7_ddio_in_inst_regouthi,
		regoutlo => wire_bidir_dq_7_ddio_in_inst_regoutlo
	  );
	bidir_dq_0_output_ddio_out_inst :  arriaii_ddio_out
	  GENERIC MAP (
		async_mode => "clear",
		half_rate_mode => "false",
		power_up => "low",
		sync_mode => "none",
		use_new_clocking_model => "true"
	  )
	  PORT MAP ( 
		areset => bidir_dq_areset(0),
		clkhi => dq_output_reg_clk,
		clklo => dq_output_reg_clk,
		datainhi => bidir_dq_output_data_in_high(0),
		datainlo => bidir_dq_output_data_in_low(0),
		dataout => wire_bidir_dq_0_output_ddio_out_inst_dataout,
		muxsel => dq_output_reg_clk
	  );
	bidir_dq_1_output_ddio_out_inst :  arriaii_ddio_out
	  GENERIC MAP (
		async_mode => "clear",
		half_rate_mode => "false",
		power_up => "low",
		sync_mode => "none",
		use_new_clocking_model => "true"
	  )
	  PORT MAP ( 
		areset => bidir_dq_areset(1),
		clkhi => dq_output_reg_clk,
		clklo => dq_output_reg_clk,
		datainhi => bidir_dq_output_data_in_high(1),
		datainlo => bidir_dq_output_data_in_low(1),
		dataout => wire_bidir_dq_1_output_ddio_out_inst_dataout,
		muxsel => dq_output_reg_clk
	  );
	bidir_dq_2_output_ddio_out_inst :  arriaii_ddio_out
	  GENERIC MAP (
		async_mode => "clear",
		half_rate_mode => "false",
		power_up => "low",
		sync_mode => "none",
		use_new_clocking_model => "true"
	  )
	  PORT MAP ( 
		areset => bidir_dq_areset(2),
		clkhi => dq_output_reg_clk,
		clklo => dq_output_reg_clk,
		datainhi => bidir_dq_output_data_in_high(2),
		datainlo => bidir_dq_output_data_in_low(2),
		dataout => wire_bidir_dq_2_output_ddio_out_inst_dataout,
		muxsel => dq_output_reg_clk
	  );
	bidir_dq_3_output_ddio_out_inst :  arriaii_ddio_out
	  GENERIC MAP (
		async_mode => "clear",
		half_rate_mode => "false",
		power_up => "low",
		sync_mode => "none",
		use_new_clocking_model => "true"
	  )
	  PORT MAP ( 
		areset => bidir_dq_areset(3),
		clkhi => dq_output_reg_clk,
		clklo => dq_output_reg_clk,
		datainhi => bidir_dq_output_data_in_high(3),
		datainlo => bidir_dq_output_data_in_low(3),
		dataout => wire_bidir_dq_3_output_ddio_out_inst_dataout,
		muxsel => dq_output_reg_clk
	  );
	bidir_dq_4_output_ddio_out_inst :  arriaii_ddio_out
	  GENERIC MAP (
		async_mode => "clear",
		half_rate_mode => "false",
		power_up => "low",
		sync_mode => "none",
		use_new_clocking_model => "true"
	  )
	  PORT MAP ( 
		areset => bidir_dq_areset(4),
		clkhi => dq_output_reg_clk,
		clklo => dq_output_reg_clk,
		datainhi => bidir_dq_output_data_in_high(4),
		datainlo => bidir_dq_output_data_in_low(4),
		dataout => wire_bidir_dq_4_output_ddio_out_inst_dataout,
		muxsel => dq_output_reg_clk
	  );
	bidir_dq_5_output_ddio_out_inst :  arriaii_ddio_out
	  GENERIC MAP (
		async_mode => "clear",
		half_rate_mode => "false",
		power_up => "low",
		sync_mode => "none",
		use_new_clocking_model => "true"
	  )
	  PORT MAP ( 
		areset => bidir_dq_areset(5),
		clkhi => dq_output_reg_clk,
		clklo => dq_output_reg_clk,
		datainhi => bidir_dq_output_data_in_high(5),
		datainlo => bidir_dq_output_data_in_low(5),
		dataout => wire_bidir_dq_5_output_ddio_out_inst_dataout,
		muxsel => dq_output_reg_clk
	  );
	bidir_dq_6_output_ddio_out_inst :  arriaii_ddio_out
	  GENERIC MAP (
		async_mode => "clear",
		half_rate_mode => "false",
		power_up => "low",
		sync_mode => "none",
		use_new_clocking_model => "true"
	  )
	  PORT MAP ( 
		areset => bidir_dq_areset(6),
		clkhi => dq_output_reg_clk,
		clklo => dq_output_reg_clk,
		datainhi => bidir_dq_output_data_in_high(6),
		datainlo => bidir_dq_output_data_in_low(6),
		dataout => wire_bidir_dq_6_output_ddio_out_inst_dataout,
		muxsel => dq_output_reg_clk
	  );
	bidir_dq_7_output_ddio_out_inst :  arriaii_ddio_out
	  GENERIC MAP (
		async_mode => "clear",
		half_rate_mode => "false",
		power_up => "low",
		sync_mode => "none",
		use_new_clocking_model => "true"
	  )
	  PORT MAP ( 
		areset => bidir_dq_areset(7),
		clkhi => dq_output_reg_clk,
		clklo => dq_output_reg_clk,
		datainhi => bidir_dq_output_data_in_high(7),
		datainlo => bidir_dq_output_data_in_low(7),
		dataout => wire_bidir_dq_7_output_ddio_out_inst_dataout,
		muxsel => dq_output_reg_clk
	  );
	dqs_0_output_ddio_out_inst :  arriaii_ddio_out
	  GENERIC MAP (
		async_mode => "none",
		half_rate_mode => "false",
		sync_mode => "none",
		use_new_clocking_model => "true"
	  )
	  PORT MAP ( 
		clkhi => dqs_output_reg_clk,
		clklo => dqs_output_reg_clk,
		datainhi => dqs_output_data_in_high(0),
		datainlo => dqs_output_data_in_low(0),
		dataout => wire_dqs_0_output_ddio_out_inst_dataout,
		muxsel => dqs_output_reg_clk
	  );
	output_dq_0_output_ddio_out_inst :  arriaii_ddio_out
	  GENERIC MAP (
		async_mode => "clear",
		half_rate_mode => "false",
		sync_mode => "none",
		use_new_clocking_model => "true"
	  )
	  PORT MAP ( 
		clkhi => dq_output_reg_clk,
		clklo => dq_output_reg_clk,
		datainhi => output_dq_output_data_in_high(0),
		datainlo => output_dq_output_data_in_low(0),
		dataout => wire_output_dq_0_output_ddio_out_inst_dataout,
		muxsel => dq_output_reg_clk
	  );
	dqs_0_delay_chain_inst :  arriaii_dqs_delay_chain
	  GENERIC MAP (
		delay_buffer_mode => "high",
		dqs_ctrl_latches_enable => "false",
		dqs_input_frequency => "400.0 MHz",
		dqs_offsetctrl_enable => "false",
		dqs_phase_shift => 9000,
		phase_setting => 2
	  )
	  PORT MAP ( 
		delayctrlin => dll_delayctrlin,
		dqsbusout => wire_dqs_0_delay_chain_inst_dqsbusout,
		dqsin => dqs_input_data_in(0)
	  );
	dqs_0_enable_inst :  arriaii_dqs_enable
	  PORT MAP ( 
		dqsbusout => wire_dqs_0_enable_inst_dqsbusout,
		dqsenable => wire_dqs_0_enable_ctrl_inst_dqsenableout,
		dqsin => wire_dqs_0_delay_chain_inst_dqsbusout
	  );
	dqs_0_enable_ctrl_inst :  arriaii_dqs_enable_ctrl
	  GENERIC MAP (
		delay_dqs_enable_by_half_cycle => "true"
	  )
	  PORT MAP ( 
		clk => dqs_enable_ctrl_clk,
		dqsenablein => dqs_enable_ctrl_in,
		dqsenableout => wire_dqs_0_enable_ctrl_inst_dqsenableout
	  );

 END RTL; --ddr3_mem_phy_alt_mem_phy_dq_dqs
--VALID FILE
