`include "if_wb_master.svh"
`include "if_wb_slave.svh"
`include "if_wb_link.svh"
`include "wb_packet_source.svh"
`include "wb_packet_sink.svh"

`include "drivers/simdrv_minic.svh"
`include "ep2ep_wrapper.svh"



module main;

   reg clk_ref  = 1'b0;
   reg clk_sys  = 1'b0;
   
   reg rst_n    = 1'b0;

   always #3900ps clk_ref <= ~clk_ref;
   always #8ns clk_sys <= ~clk_sys;

   
   initial begin
      repeat(3) @(posedge clk_sys);
      rst_n <= 1'b1;
   end

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
       .g_addr_width(32))
   U_sys_bus_master
     (
      .clk_i(clk_sys),
      .rst_n_i(rst_n)
      );


   IWishboneMaster 
     #(
       .g_data_width(32),
       .g_addr_width(16))
   U_pmem_bus_master
     (
      .clk_i(clk_sys),
      .rst_n_i(rst_n)
      );


   
   IWishboneLink #(16,2) ep2minic();
   IWishboneLink #(16,2) minic2ep();

   ITXTSU_Bus dummy();
   ITXTSU_Bus minic_ts();
   
   
   ep2ep_wrapper U_Eps
     (
      .clk_sys_i(clk_sys),
      .clk_ref_i(clk_ref),
      .rst_n_i(rst_n),
      .snk_a(U_wrf_source.master),
      .src_a(U_wrf_sink.slave),
      .snk_b(minic2ep.master),
      .src_b(ep2minic.slave),
      .txts_a(dummy.endpoint),
      .txts_b(minic_ts.endpoint)
      );
   

   wire minic_irq;
   wire [31:0] pmem_wr_data, pmem_rd_data;
   wire [13:0] pmem_addr;
   wire pmem_wr;
   
   wr_mini_nic DUT 
     (
      .clk_sys_i (clk_sys),
      .rst_n_i   (rst_n),

      .mem_data_o (pmem_wr_data),
      .mem_addr_o (pmem_addr),
      .mem_data_i (pmem_rd_data),
      .mem_wr_o   (pmem_wr),

      .src_dat_o  (minic2ep.slave.dat_i),
      .src_adr_o  (minic2ep.slave.adr),
      .src_sel_o  (minic2ep.slave.sel),
      .src_cyc_o  (minic2ep.slave.cyc),
      .src_stb_o  (minic2ep.slave.stb),
      .src_we_o   (minic2ep.slave.we),
      .src_stall_i(minic2ep.slave.stall),
      .src_err_i  (minic2ep.slave.err),
      .src_ack_i  (minic2ep.slave.ack),

      .snk_dat_i  (ep2minic.dat_o), 
      .snk_adr_i  (ep2minic.adr),
      .snk_sel_i  (ep2minic.sel),
      .snk_cyc_i  (ep2minic.cyc),
      .snk_stb_i  (ep2minic.stb), 
      .snk_we_i    (ep2minic.we),
      .snk_stall_o (ep2minic.stall),
      .snk_err_o   (ep2minic.err),
      .snk_ack_o   (ep2minic.ack),

      .txtsu_port_id_i  (5'b0),
      .txtsu_frame_id_i (16'b0),
      .txtsu_tsval_i   (32'b0),
      .txtsu_valid_i   (1'b0),
      .txtsu_ack_o     (),

      

      .wb_cyc_i   (U_sys_bus_master.cyc),
      .wb_stb_i   (U_sys_bus_master.stb),
      .wb_we_i    (U_sys_bus_master.we),
      .wb_sel_i   (U_sys_bus_master.sel),
      .wb_adr_i   (U_sys_bus_master.adr),
      .wb_dat_i   (U_sys_bus_master.dat_o),
      .wb_dat_o   (U_sys_bus_master.dat_i),
      .wb_ack_o   (U_sys_bus_master.ack),
      .wb_stall_o (U_sys_bus_master.stall),
      .wb_irq_o   (minic_irq)
      );

   minic_packet_buffer PBUF
     (
      .clk_sys_i (clk_sys),
      .rst_n_i  (rst_n),

      .minic_addr_i (pmem_addr),
      .minic_data_i (pmem_wr_data),
      .minic_wr_i   (pmem_wr),
      .minic_data_o (pmem_rd_data),
      
      .wb_cyc_i   (U_pmem_bus_master.cyc),
      .wb_stb_i   (U_pmem_bus_master.stb),
      .wb_we_i    (U_pmem_bus_master.we),
      .wb_addr_i   (U_pmem_bus_master.adr[13:0]),
      .wb_data_i   (U_pmem_bus_master.dat_o),
      .wb_data_o   (U_pmem_bus_master.dat_i),
      .wb_ack_o   (U_pmem_bus_master.ack)
    );

   
   CSimDrv_Minic minic;



   task test_tx_path(int n_packets, CSimDrv_Minic minic, WBPacketSink sink);
      EthPacketGenerator gen = new;
      EthPacket pkt, tmpl;
      EthPacket txed[$];
      int i;

       tmpl           = new;
      tmpl.src       = '{1,2,3,4,5,6};
      tmpl.dst       = '{10,11,12,13,14,15};
      tmpl.has_smac  = 1;
      tmpl.is_q      = 0;
      
      gen.set_randomization(EthPacketGenerator::SEQ_PAYLOAD | EthPacketGenerator::ETHERTYPE | EthPacketGenerator::TX_OOB | EthPacketGenerator::EVEN_LENGTH) ;
      gen.set_template(tmpl);
      gen.set_size(100,200);
      
      for(i=0;i<n_packets;i++)
        begin
           pkt  = gen.gen();
           minic.send(pkt);
           txed.push_back(pkt);
        end

      fork
         forever
           begin
              minic.run();
              #1;
           end

         forever
           begin
            if(sink.poll())
              begin
                 EthPacket rxp, sent;
                 sink.recv(rxp);
                 sent  = txed.pop_front();
                 if(!sent.equal(rxp))
                   begin
                      sent.dump();
                      rxp.dump();
                      $stop;
                   end
              end
              #1;
         end
      join

   endtask
   
   
   
   initial begin
      CWishboneAccessor sys_bus, pmem_bus;     
      WBPacketSource src  = new(U_wrf_source.get_accessor());
      WBPacketSink sink   = new(U_wrf_sink.get_accessor()); 
      EthPacketGenerator gen = new;
      EthPacket pkt, tmpl;
      EthPacket txed[$];
      int i, cnt;

    
      

   
      
      
      @(posedge rst_n);
      @(posedge clk_sys);
 
      sys_bus                                 = U_sys_bus_master.get_accessor();
      sys_bus.set_mode(CLASSIC);
      pmem_bus                                 = U_pmem_bus_master.get_accessor();
      pmem_bus.set_mode(CLASSIC);

      minic  = new('h10000, sys_bus, 0, pmem_bus, 0);
      minic.init();


   //   test_tx_path(3000, minic, sink);
      

     
         tmpl           = new;
      tmpl.src       = '{1,2,3,4,5,6};
      tmpl.dst       = '{10,11,12,13,14,15};
      tmpl.has_smac  = 1;
      tmpl.is_q      = 0;
      
      gen.set_randomization(EthPacketGenerator::SEQ_PAYLOAD | EthPacketGenerator::ETHERTYPE /*| EthPacketGenerator::RX_OOB*/) ;
      gen.set_template(tmpl);
      gen.set_size(60,150);

      #10us;
      
      
      cnt  = 0;
      
      fork
         forever
           begin
              minic.run();
      

              if(minic.poll())
              begin
                 EthPacket rxp, sent;
                 minic.recv(rxp);
                 sent  = txed.pop_front();
                 cnt ++;
                 
                 if(!sent.equal(rxp, EthPacket::CMP_OOB))
                   begin
                      sent.dump();
                      rxp.dump();
                      $stop;
                   end

                 else
                   $display("Rx: %x cnt %d", rxp.ethertype, cnt);
                 

                 
              end

              #1;
              end

//         forever
           begin
              for(i=0;i<205;i++)
                begin
                   pkt  = gen.gen();
                   src.send(pkt);
                   txed.push_back(pkt);
                end

              
           end
         
    //          #1;
              
  //         end // forever begin

         
         
         
              

      join      

         
      
        
      
   end // initial begin

   
   
endmodule // main



