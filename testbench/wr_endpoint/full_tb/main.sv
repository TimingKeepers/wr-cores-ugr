`timescale 1ns/1ps

`include "endpoint_regs.v"
`include "endpoint_mdio.v"
`include "tbi_utils.sv"

`include "if_wb_master.svh"
`include "if_wb_slave.svh"
`include "wb_packet_source.svh"
`include "wb_packet_sink.svh"

`timescale 1ps/1ps

`include "old_endpoint_wrapper.svh"
`include "endpoint_phy_wrapper.svh"


`include "pfilter.svh"




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
   endtask // pfilter_enable

   task automatic mdio_read(int base, int addr, output int val);
      reg[31:0] rval;

      m_acc.write(base+`ADDR_EP_MDIO_CR, (addr>>2) << 16);
      while(1)begin
	 m_acc.read(base+`ADDR_EP_MDIO_SR, rval);
	 if(rval[31]) begin
	    val  = rval[15:0];
	    return;
	 end
      end
   endtask // mdio_read

   task automatic mdio_write(int base, int addr,int val);
      reg[31:0] rval;

      m_acc.write(base+`ADDR_EP_MDIO_CR, (addr>>2) << 16 | `EP_MDIO_CR_RW);
      while(1)begin
	 m_acc.read(base+`ADDR_EP_MDIO_SR, rval);
	 if(rval[31])
	   return;
      end
   endtask // automatic

        

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
       .g_sysclk_period(8011),
       .g_rbclk_period(8000),
       .g_refclk_period(8000)
       )
   U_Clock_Gen 
       (
        .clk_ref_o (clk_ref_new),
        .clk_sys_o (clk_sys),
        .phy_rbclk_o (clk_ref_old),
        .rst_n_o(rst_n)
        );

   wire rxn,rxp,txn,txp;
   wire [9:0] td;
   wire [9:0] rd;
   
   
   old_endpoint_test_wrapper
     #(
       .g_phy_type("TBI"))
   U_oldep_wrap   
     (
      .clk_sys_i(clk_sys),
      
      .clk_ref_i(clk_ref_old),
      .clk_rx_i(clk_ref_new),
      .rst_n_i(rst_n),

      .td_o(td),
      .rd_i(rd)
     
/* -----\/----- EXCLUDED -----\/-----
      .rxp_i(rxp),
      .rxn_i(rxn),

      .txp_o(txp),
      .txn_o(txn)
 -----/\----- EXCLUDED -----/\----- */
      );

   reg clk_ref_gtx                       = 1;

   always@(posedge clk_ref_new) clk_ref_gtx <= ~clk_ref_gtx;
   
   endpoint_phy_wrapper
     #(
       .g_phy_type("TBI")) 
   U_Wrapped_EP
     (
      .clk_sys_i(clk_sys),
      .clk_ref_i(clk_ref_new),    
      .clk_rx_i(clk_ref_new),
      .rst_n_i(rst_n),

      .snk (U_wrf_sink.slave),
      .src(U_wrf_source.master),
      .sys(U_sys_bus_master.master),

      .td_o(rd),
      .rd_i(td)
      
/* -----\/----- EXCLUDED -----\/-----
      .txn_o(rxn),
      .txp_o(rxp),

      .rxn_i(txn),
      .rxp_i(txp)
 -----/\----- EXCLUDED -----/\----- */
      );

   
   task check_disparity8(bit[7:0] d, bit k);
      static bit[7:0] prev_d[4];
      static bit prev_k[4];
      int i;
      
      prev_k[0]    <= k;
      prev_d[0]    <= d;
      for(i=1;i<4;i++)begin
         prev_k[i] <= prev_k[i-1];
         prev_d[i] <= prev_d[i-1];
      end

      if(prev_k[0] && prev_d[0] == 'hbc && prev_k[1] && prev_d[1] == 'hf7)
        begin
           $display("EndingDisp: %x", d);
        end
   endtask // record_disparity
   
   task check_disparity16(bit[15:0] d, bit[1:0] k);
      static bit[15:0] prev_d[4];
      static bit[1:0] prev_k[4];
      int i;
      
      prev_k[0]    <= k;
      prev_d[0]    <= d;
      for(i=1;i<4;i++)begin
         prev_k[i] <= prev_k[i-1];
         prev_d[i] <= prev_d[i-1];
      end

      if(prev_k[0][1] && prev_d[0][15:8] == 'hbc && prev_k[1][0] && prev_d[1][7:0] == 'hf7)
        begin
           $display("EndingDisp: %x", prev_d[0]);
        end
   endtask // record_disparity
   

   task automatic tx_test(int n_tries, int is_q, int unvid, EthPacketSource src, EthPacketSink sink);
      EthPacketGenerator gen = new;
      EthPacket pkt, tmpl, pkt2;
      EthPacket arr[];
      int i;
      
      arr            = new[n_tries](arr);
      
  
      tmpl           = new;
      tmpl.src       = '{1,2,3,4,5,6};
      tmpl.dst       = '{'h00, 'h50, 'hca, 'hfe, 'hba, 'hbe};
      tmpl.has_smac  = 1;
      tmpl.is_q      = is_q;
      tmpl.vid       = 100;
      tmpl.ethertype = 'h88f7;
  // 
      gen.set_randomization(EthPacketGenerator::SEQ_PAYLOAD /* | EthPacketGenerator::TX_OOB*/) ;
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
   
   task init_pfilter(CSimDrv_WR_Endpoint ep_drv);
      PFilterMicrocode mc  = new;

      mc.cmp(0, 'hffff, 'hffff, PFilterMicrocode::MOV, 1);
      mc.cmp(1, 'hffff, 'hffff, PFilterMicrocode::AND, 1);
      mc.cmp(2, 'hffff, 'hffff, PFilterMicrocode::AND, 1);
      mc.cmp(0, 'h011b, 'hffff, PFilterMicrocode::MOV, 2);
      mc.cmp(1, 'h1900, 'hffff, PFilterMicrocode::AND, 2);
      mc.cmp(2, 'h0000, 'hffff, PFilterMicrocode::AND, 2);
      mc.cmp(0, 'h0050, 'hffff, PFilterMicrocode::MOV, 3);
      mc.cmp(1, 'hcafe, 'hffff, PFilterMicrocode::AND, 3);
      mc.cmp(2, 'hbabe, 'hffff, PFilterMicrocode::AND, 3);
      mc.cmp(6, 'ha0a0, 'hffff, PFilterMicrocode::MOV, 4);
      mc.cmp(6, 'h88f7, 'hffff, PFilterMicrocode::MOV, 5);

      mc.logic3(7, 3, PFilterMicrocode::AND, 4, PFilterMicrocode::OR, 5);
      mc.logic2(23, 7, PFilterMicrocode::NOT, 0);
      mc.logic2(31, 3, PFilterMicrocode::AND, 4);
      mc.logic2(24, 5, PFilterMicrocode::MOV, 0);
      
      
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

      #200us;
      
      

      U_wrf_sink.settings.gen_random_stalls   = 1;
      U_wrf_sink.settings.stall_prob          = 0.01;
      U_wrf_sink.settings.stall_min_duration  = 5;
      U_wrf_sink.settings.stall_max_duration  = 30;
      
        
      sys_bus                                 = U_sys_bus_master.get_accessor();
      
      sys_bus.set_mode(CLASSIC);
      sys_bus.write(`ADDR_EP_ECR, `EP_ECR_TX_EN | `EP_ECR_RX_EN);
      sys_bus.write(`ADDR_EP_RFCR, 1518 << `EP_RFCR_MRU_OFFSET);
      sys_bus.write(`ADDR_EP_VCR0, `EP_QMODE_VLAN_DISABLED << `EP_VCR0_QMODE_OFFSET);
      sys_bus.write(`ADDR_EP_TSCR, `EP_TSCR_EN_RXTS);
      

      ep_drv                                       = new(CBusAccessor'(sys_bus), 0);
      

      o_sink                                         = U_oldep_wrap.sink;
      o_src                                        = U_oldep_wrap.src;
      
            //     pkt.dump();
      U_wrf_source.settings.gen_random_throttling  = 1;
      U_wrf_source.settings.throttle_prob          = 0.02;
      
      ep_drv.vlan_egress_untag(100, 1);
         
/*      for(i=0;i<10;i++)
        begin
           $display("Iter: %d", i);
           tx_test(100, 0, 0, src, o_sink);
           tx_test(100, 1, 1, src, o_sink);
        end // initial begin

      #10000ns;

      $stop;*/
     

      init_pfilter(ep_drv);
      
      for(i=0;i<10;i++)
        begin
           $display("RX Iter %d", i);
           tx_test(5, 0, 0, o_src, sink);
/* -----\/----- EXCLUDED -----\/-----
           $display("TX Iter %d", i);
           tx_test(5, 0, 0, src, o_sink);
 -----/\----- EXCLUDED -----/\----- */
        end
      
   end // initial begin
   
 /*  always@(posedge clk_ref_new)
     begin
        check_disparity8(U_oldep_wrap.gtx_data, U_oldep_wrap.gtx_k);
     end*/

/*   always@(posedge U_Wrapped_EP.tx_clock)
     begin
        check_disparity16(U_Wrapped_EP.gtx_data, U_Wrapped_EP.gtx_k);
     end*/ 
   
  
   
endmodule // main



