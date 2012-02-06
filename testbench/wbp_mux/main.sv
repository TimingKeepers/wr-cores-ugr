`include "if_wb_master.svh"
`include "if_wb_slave.svh"
`include "if_wb_link.svh"
`include "wb_packet_source.svh"
`include "wb_packet_sink.svh"

`define WIRE_WB_SINK(iface, prefix) \
.prefix``_adr_i(iface.adr), \
.prefix``_dat_i(iface.dat_o), \
.prefix``_stb_i(iface.stb), \
.prefix``_sel_i(iface.sel), \
.prefix``_cyc_i(iface.cyc), \
.prefix``_ack_o(iface.ack), \
.prefix``_err_o(iface.err), \
.prefix``_stall_o(iface.stall)
  

`define WIRE_WB_SOURCE(iface, prefix) \
.prefix``_adr_o(iface.adr), \
.prefix``_dat_o(iface.dat_i), \
.prefix``_stb_o(iface.stb), \
.prefix``_sel_o(iface.sel), \
.prefix``_cyc_o(iface.cyc), \
.prefix``_ack_i(iface.ack), \
.prefix``_err_i(iface.err), \
.prefix``_stall_i(iface.stall)



        
module mux_svwrap
    (
           input clk_sys_i, 
           input rst_n_i
           );

   IWishboneMaster #(2,16) U_ep_src (clk_sys_i, rst_n_i);
   IWishboneMaster #(2,16) U_minic_src (clk_sys_i, rst_n_i);
   IWishboneMaster #(2,16) U_ext_src (clk_sys_i, rst_n_i);
   IWishboneSlave #(2,16) U_ep_snk (clk_sys_i, rst_n_i);
   IWishboneSlave #(2,16) U_minic_snk (clk_sys_i, rst_n_i);
   IWishboneSlave #(2,16) U_ext_snk (clk_sys_i, rst_n_i);

   wbp_mux 
     U_Mux
       (
        .clk_sys_i (clk_sys_i),
        .rst_n_i   (rst_n_i),

        `WIRE_WB_SINK(U_ep_src, ep_wbs),
        `WIRE_WB_SOURCE(U_ep_snk, ep_wbm),

        `WIRE_WB_SINK(U_minic_src, ptp_wbs),
        `WIRE_WB_SOURCE(U_minic_snk, ptp_wbm),

        `WIRE_WB_SINK(U_ext_src, ext_wbs),
        `WIRE_WB_SOURCE(U_ext_snk, ext_wbm),

        .class_core_i (8'hf0)
        );

   assign U_ep_snk.we  = 1;
   assign U_minic_snk.we  = 1;
   assign U_ext_snk.we  = 1;
   
   
   WBPacketSource ep_src, minic_src, ext_src;
   WBPacketSink ep_snk, minic_snk, ext_snk;
   
   initial begin
      @(posedge rst_n_i);
      @(posedge clk_sys_i);

      ep_src  = new(U_ep_src.get_accessor());
      minic_src  = new(U_minic_src.get_accessor());
      ext_src  = new(U_ext_src.get_accessor());

      ep_snk  = new(U_ep_snk.get_accessor());
      minic_snk  = new(U_minic_snk.get_accessor());
      ext_snk  = new(U_ext_snk.get_accessor());
      
      
   end
   
endmodule // mux_svwrap

module main;
   reg clk_ref  = 1'b0;
   wire clk_sys  ;
   reg rst_n    = 1'b0;

   always #4ns clk_ref <= ~clk_ref;
   assign clk_sys = clk_ref;
   
   initial begin
      repeat(3) @(posedge clk_sys);
      rst_n <= 1'b1;
   end

   mux_svwrap DUT (clk_sys, rst_n);


   task verify_rx_queue(WBPacketSink snk, EthPacket q[$], output int n_packets);
      automatic int n;
      n  =0;
      
      
      while(snk.poll())
        begin
           EthPacket from_q, pkt;
           n++;
           snk.recv(pkt);
           from_q  = q.pop_front();



           if(!pkt.equal(from_q))
             begin
                pkt.dump();
                from_q.dump();                $stop;
             end
        end

      n_packets  = n;
   endtask // verify_rx_queue
   
  task automatic send_random_packets(WBPacketSource src,ref EthPacket q[$], input int n_packets,  input int pclass);
     EthPacket pkt, tmpl;
     EthPacketGenerator gen  = new;
     int i;
     

   tmpl                      = new;
      tmpl.src               = '{1,2,3,4,5,6};
      tmpl.dst               = '{10,11,12,13,14,15};
      tmpl.has_smac          = 1;
      tmpl.is_q              = 0;
      
      gen.set_randomization(EthPacketGenerator::SEQ_PAYLOAD | EthPacketGenerator::ETHERTYPE /*| EthPacketGenerator::RX_OOB*/) ;
      gen.set_template(tmpl);
      gen.set_size(46, 1000);

     for(i=0;i<n_packets;i++)
       begin
          pkt         = gen.gen();
          pkt.pclass  = pclass;
          
          q.push_back(pkt);
          src.send(pkt);
       end
   endtask // send_random_packets
   
   task test_classifier(int n_packets);
      int i, seed = 0,n1=0,n2=0;
      EthPacket pkt, tmpl;
      EthPacket to_ext[$], to_minic[$];
      EthPacketGenerator gen  = new;

      tmpl                = new;
      tmpl.src                = '{1,2,3,4,5,6};
      tmpl.dst                = '{10,11,12,13,14,15};
      tmpl.has_smac           = 1;
      tmpl.is_q               = 0;
      
      gen.set_randomization(EthPacketGenerator::SEQ_PAYLOAD | EthPacketGenerator::ETHERTYPE /*| EthPacketGenerator::RX_OOB*/) ;
      gen.set_template(tmpl);
      gen.set_size(46, 1000);

      for(i=0;i<n_packets;i++)
        begin
           pkt         = gen.gen();
           pkt.pclass  = (1<<$dist_uniform(seed,0,7));
           if(pkt.pclass & 'hf0)
             to_minic.push_back(pkt);
           else
             to_ext.push_back(pkt);
           DUT.ep_src.send(pkt);
           end

      
      verify_rx_queue(DUT.ext_snk, to_ext, n1);

      verify_rx_queue(DUT.minic_snk, to_minic, n2);

      if(n1+n2 != n_packets)
        $error("FAILURE n1 %d n2 %d n_packets %d", n1, n2, n_packets);
      else
        $display("PASS");
      
      
   endtask // test_classifier

 
   
        
   task automatic test_arbiter(int n_packets);
      int n, n1, n2;
      EthPacket from_ext[$], from_minic[$], pkt;

      n   = 0;
      n1  = 0;
      n2  = 0;
      

      fork
         send_random_packets(DUT.ext_src, from_ext, n_packets, 1);
         send_random_packets(DUT.minic_src, from_minic, n_packets, 2);
      join
      

      while(DUT.ep_snk.poll())
        begin
           EthPacket from_q;
           
           DUT.ep_snk.recv(pkt);
           if(pkt.pclass == 1)
             begin
                from_q  = from_ext.pop_front();
                n1++;
             end
           else
             begin
                from_q  = from_minic.pop_front();
                n2++;
             end

           if(!from_q.equal(pkt))
             begin
                from_q.dump();
                pkt.dump();
                $error("FAIL at %d (%d,%d)\n", n,n1,n2);
                
                
                break;
                
                
             end
           n++;
           
           
        end // while (DUT.ep_snk.poll())
      $display("PASS");
      
      
      
   endtask // test_arbiter
   
         
      
    
 
   
   
   initial begin
      int i;
      EthPacket pkt, tmpl;
      EthPacketGenerator gen  = new;
      
      @(posedge rst_n);
      @(posedge clk_sys);
      test_classifier(100);
//      test_arbiter(10000);
      
      
   end
   
      



endmodule // main

