/* Some basic definitions: types, abstract BusAccessor class */
`include "simdrv_defs.svh"

`include "if_wb_master.svh"
`include "if_wb_slave.svh"
`include "wb_packet_source.svh"
`include "wb_packet_sink.svh"

   
module main;

   wire m_cyc,m_we,m_stb;
   wire[3:0]m_sel;
   wire[31:0]m_adr,m_wrdat;
   logic [31:0] m_rddat;
   logic m_ack;

/* clock & reset generator */   
   reg clk_sys  = 1'b0;
   reg rst_n    = 1'b0;

   always #5ns clk_sys <= ~clk_sys;
   initial begin
      repeat(3) @(posedge clk_sys);
      rst_n <= 1'b1;
   end

/* A wishbone master, sending the packets to the MiniBone.
   Controlled via child CWishboneAccessor object. */
   IWishboneMaster 
     #(
       .g_data_width(16),
       .g_addr_width(2))
   U_source
     (
      .clk_i(clk_sys),
      .rst_n_i(rst_n)
      );

   /* A Wishbone slave. Receives the packets produced by the MiniBone. */
   IWishboneSlave
     #(
       .g_data_width(16),
       .g_addr_width(2))
   U_sink
     (
      .clk_i(clk_sys),
      .rst_n_i(rst_n)
      );
   
   mini_bone
     #(
       .g_class_mask(8'hff),
       .g_our_ethertype('ha0a0)) 
   DUT
     (
      .clk_sys_i (clk_sys),
      .rst_n_i   (rst_n),

      /* Packet I/O - from the Endpoint to the MB */
      
      .snk_cyc_i  (U_source.master.cyc),
      .snk_stb_i  (U_source.master.stb),
      .snk_dat_i  (U_source.master.dat_o),
      .snk_sel_i  (U_source.master.sel),
      .snk_adr_i  (U_source.master.adr),
      .snk_we_i   (U_source.master.we),
      .snk_stall_o(U_source.master.stall),
      .snk_ack_o  (U_source.master.ack),

      /* Packet I/O - from the MB to the Endpoint */
      .src_cyc_o   (U_sink.slave.cyc),
      .src_stb_o   (U_sink.slave.stb),
      .src_dat_o   (U_sink.slave.dat_i),
      .src_adr_o   (U_sink.slave.adr),
      .src_sel_o   (U_sink.slave.sel),
      .src_we_o    (U_sink.slave.we),
      .src_ack_i   (U_sink.slave.ack),
      .src_stall_i (U_sink.slave.stall),

      /* WB Master driving the memory */
      .master_cyc_o  (m_cyc),
      .master_we_o   (m_we),
      .master_stb_o  (m_stb),
      .master_sel_o  (m_sel),
      .master_adr_o  (m_adr),
      .master_dat_o  (m_wrdat),
      .master_dat_i  (m_rddat),
      .master_ack_i   (m_ack));
   
   logic [31:0] mem[65536];

   /* a trivial wishbone memory model */
   always@(posedge clk_sys)
     if(!rst_n) begin
        m_ack   <= 0;
        m_rddat <= 0;
     end else begin
        if(m_ack) 
          m_ack <= 0;
        else if(m_cyc && m_stb) begin
           if(m_we)
             begin
            /* $display("MemWrite: addr %x data %x", m_adr, m_wrdat); */
             mem[m_adr[15:0]] <= m_wrdat;
             end
           m_rddat       <= mem[m_adr[15:0]];
           m_ack         <= 1;
        end
     end

   /* Packet Source and Sink objects - these objects translate Ethernet packets into/from
      Wishbone bus transactions, extracting the statuses and OOB */
   WBPacketSource src; 
   WBPacketSink sink;

   /* Executes a write cycle using minibone */
   task mbone_write(uint32_t addr, uint32_t data);
      EthPacket pkt;
      pkt            = new;
      
      /* some dummy addresses */
      pkt.dst        = '{'hff, 'hff, 'hff, 'hff, 'hff, 'hff};
      pkt.src        = '{1,2,3,4,5,6};
      pkt.ethertype  = 'ha0a0;

      /* set the payload size to the minimum acceptable value:
         (46 bytes payload + 14 bytes header + 4 bytes CRC) */
      pkt.set_size(46);

      /* .. and fill in the packet structure */
      pkt.payload[0]  = 0;
      pkt.payload[1]  = 'h1f; /* Flags (write = 1, SEL = 0xf) */

      pkt.payload[2]  = (addr >> 24) & 'hff;
      pkt.payload[3]  = (addr >> 16) & 'hff;
      pkt.payload[4]  = (addr >> 8) & 'hff;
      pkt.payload[5]  = (addr >> 0) & 'hff;

      pkt.payload[6]  = (data >> 24) & 'hff;
      pkt.payload[7]  = (data >> 16) & 'hff;
      pkt.payload[8]  = (data >> 8) & 'hff;
      pkt.payload[9]  = (data >> 0) & 'hff;
      
      /* send the packet */
      src.send(pkt);
      /* and receive the reply. No error handling yet. */
      sink.recv(pkt);
   endtask // mbone_write


   /* The same thing, but for reads */
   task mbone_read(uint32_t addr, output uint32_t data);
      EthPacket pkt;
      pkt            = new;
      

      pkt.dst        = '{'hff, 'hff, 'hff, 'hff, 'hff, 'hff};
      pkt.src        = '{1,2,3,4,5,6};
      pkt.ethertype  = 'ha0a0;
      pkt.set_size(46);

      pkt.payload[0]  = 0;
      pkt.payload[1]  = 'h0f;

      pkt.payload[2]  = (addr >> 24) & 'hff;
      pkt.payload[3]  = (addr >> 16) & 'hff;
      pkt.payload[4]  = (addr >> 8) & 'hff;
      pkt.payload[5]  = (addr >> 0) & 'hff;
      
      src.send(pkt);
      sink.recv(pkt);

  //    pkt.dump();

      if((pkt.payload[1] & 3) == 1)
        begin
           reg[31:0] d;
           d[31:24] = pkt.payload[2];
           d[23:16] = pkt.payload[3];
           d[15:8] = pkt.payload[4];
           d[7:0] = pkt.payload[5];
           data  = d;
        end
      
   endtask // mbone_read
   


   
   initial begin
      int i, retries;
      uint32_t rval;
      int seed;
      
      
      #1us;

      /* Create the sink/source objects - they communicate with the WB Master/Slave using an Accessor
       object: 
       Ethernet Packet (eth_packet_t) -> src->send() -> serialization -> sequence of reads/writes -> accessor -> IWishboneMaster -> device under test */
      src                                      = new(U_source.get_accessor());
      sink                                     = new(U_sink.get_accessor());

      /* Make the things not ideal */
      U_source.settings.throttle_prob          = 0.1;
      U_source.settings.gen_random_throttling  = 1; /* 10% probability of STB going low */

      U_sink.settings.stall_prob               = 0.1;
      U_sink.settings.gen_random_stalls        = 1; /* 10 % probability of STALL event, stalls 1-3 cycles long */
      U_sink.settings.stall_min_duration       = 1;
      U_sink.settings.stall_max_duration       = 3;
      

      /* try executing a bunch of random writes and verify if the written data is where it should be */

      for(retries = 0; retries <10; retries++)
        begin
           const int n_writes                  = 100;

           $display("Iteration %d", retries);
           

           seed                                = retries;
           
           for(i=0;i<n_writes;i++)
             mbone_write(i, $dist_uniform(seed, 0, (1<<31)-1));

           seed = retries;
           for(i=0;i<n_writes;i++)
             begin
                mbone_read(i, rval);
                if(rval != $dist_uniform(seed, 0, (1<<31)-1))
                
                  begin
                     $error("Inconsistency at %d", i);
                     $stop;
                  end
             end

        end
     
      $display("Test passed");
      $stop;
      
   end

endmodule // main

   
