`include "if_wb_master.svh"
`include "if_wb_slave.svh"
`include "if_wb_link.svh"
`include "wb_packet_source.svh"
`include "wb_packet_sink.svh"
import wr_fabric_pkg::*;

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


`define WIRE_WRF_SRC(dst, src) \
assign dst``_o.cyc = src.cyc; \
assign dst``_o.stb = src.stb; \
assign dst``_o.adr = src.adr; \
assign dst``_o.dat = src.dat_o; \
assign dst``_o.sel = src.sel; \
assign src.ack = dst``_i.ack; \
assign src.err = dst``_i.err; \
assign src.stall = dst``_i.stall;

`define WIRE_WRF_SRC_I(dst, src, i) \
assign dst``_o[i].cyc = src.cyc; \
assign dst``_o[i].stb = src.stb; \
assign dst``_o[i].adr = src.adr; \
assign dst``_o[i].dat = src.dat_o; \
assign dst``_o[i].sel = src.sel; \
assign src.ack = dst``_i[i].ack; \
assign src.err = dst``_i[i].err; \
assign src.stall = dst``_i[i].stall;

`define WIRE_WRF_SNK(dst, src) \
assign dst.cyc = src``_i.cyc; \
assign dst.stb = src``_i.stb; \
assign dst.adr = src``_i.adr; \
assign dst.dat_i = src``_i.dat; \
assign dst.sel = src``_i.sel; \
assign src``_o.ack = dst.ack; \
assign src``_o.err = dst.err; \
assign src``_o.stall = dst.stall;

`define WIRE_WRF_SNK_I(dst, src, i) \
assign dst.cyc = src``_i[i].cyc; \
assign dst.stb = src``_i[i].stb; \
assign dst.adr = src``_i[i].adr; \
assign dst.dat_i = src``_i[i].dat; \
assign dst.sel = src``_i[i].sel; \
assign src``_o[i].ack = dst.ack; \
assign src``_o[i].err = dst.err; \
assign src``_o[i].stall = dst.stall;

        
module mux_svwrap (
    input clk_sys_i, 
    input rst_n_i
  );

   IWishboneMaster #(2,16) U_ep_src (clk_sys_i, rst_n_i);
   IWishboneMaster #(2,16) U_minic_src (clk_sys_i, rst_n_i);
   IWishboneMaster #(2,16) U_ext_src (clk_sys_i, rst_n_i);
   IWishboneSlave  #(2,16) U_ep_snk (clk_sys_i, rst_n_i);
   IWishboneSlave  #(2,16) U_minic_snk (clk_sys_i, rst_n_i);
   IWishboneSlave  #(2,16) U_ext_snk (clk_sys_i, rst_n_i);
   IWishboneMaster #(2,16) U_mux_src (clk_sys_i, rst_n_i);
   IWishboneSlave  #(2,16) U_mux_snk (clk_sys_i, rst_n_i);

   t_wrf_source_out ep_src_o;
   t_wrf_source_in  ep_src_i;
   t_wrf_sink_out   ep_snk_o;
   t_wrf_sink_in    ep_snk_i;

   `WIRE_WRF_SRC(ep_src, U_ep_src);
   `WIRE_WRF_SNK(U_ep_snk, ep_snk);

   t_wrf_source_out mux_src_o[2:0];
   t_wrf_source_in  mux_src_i[2:0];
   t_wrf_sink_out   mux_snk_o[2:0];
   t_wrf_sink_in    mux_snk_i[2:0];

   `WIRE_WRF_SRC_I(mux_src, U_minic_src, 0);
   `WIRE_WRF_SNK_I(U_minic_snk, mux_snk, 0);
   //assign U_minic_snk.cyc = 1'b1; //mux_snk_i[0].cyc;
   //assign U_minic_snk.stb = mux_snk_i[0].stb;
   //assign mux_snk_o[0].ack = U_minic_snk.ack;
   //assign mux_snk_o[0].err= U_minic_snk.err;
   //assign mux_snk_o[0].stall = U_minic_snk.stall;
   `WIRE_WRF_SRC_I(mux_src, U_ext_src, 1);
   `WIRE_WRF_SNK_I(U_ext_snk, mux_snk, 1);
   `WIRE_WRF_SRC_I(mux_src, U_mux_src, 2);
   `WIRE_WRF_SNK_I(U_mux_snk, mux_snk, 2);

   reg [7:0]muxclass[2:0] = {8'h03, 8'h0c, 8'hf0};

   xwrf_mux 
    #(
      .g_muxed_ports(3))
   U_Mux
    (
      .clk_sys_i (clk_sys_i),
      .rst_n_i   (rst_n_i),

      .ep_snk_i (ep_src_o),
      .ep_snk_o (ep_src_i),
      .ep_src_i (ep_snk_o),
      .ep_src_o (ep_snk_i),

      .mux_snk_i(mux_src_o),
      .mux_snk_o(mux_src_i),
      .mux_src_i(mux_snk_o),
      .mux_src_o(mux_snk_i),

      .mux_class_i (muxclass)
    );

   assign U_ep_snk.we  = 1;
   assign U_minic_snk.we  = 1;
   assign U_ext_snk.we  = 1;
   assign U_mux_snk.we  = 1;
   
   
   WBPacketSource ep_src, minic_src, ext_src, mux_src;
   WBPacketSink ep_snk, minic_snk, ext_snk, mux_snk;
   
   initial begin
      @(posedge rst_n_i);
      @(posedge clk_sys_i);

      ep_src  = new(U_ep_src.get_accessor());
      minic_src  = new(U_minic_src.get_accessor());
      ext_src  = new(U_ext_src.get_accessor());
      mux_src  = new(U_mux_src.get_accessor());

      ep_snk  = new(U_ep_snk.get_accessor());
      minic_snk  = new(U_minic_snk.get_accessor());
      ext_snk  = new(U_ext_snk.get_accessor());
      mux_snk  = new(U_mux_snk.get_accessor());
      
      U_ep_src.settings.cyc_on_stall = 1;
      U_minic_src.settings.cyc_on_stall = 1;
      U_ext_src.settings.cyc_on_stall = 1;
      U_mux_src.settings.cyc_on_stall = 1;
      
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
     

      tmpl                   = new;
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
         send_random_packets(DUT.mux_src, from_minic, n_packets, 3);
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
//      test_classifier(100);
      test_arbiter(100);
      
      
   end
   
      



endmodule // main

