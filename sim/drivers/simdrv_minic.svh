`include "simdrv_defs.svh"
`include "eth_packet.svh"
`include "minic_regs.vh"

`define MAX_PACKET_SIZE 1536

class CSimDrv_Minic;

   const uint32_t TX_DESC_VALID        = (1<<31);
   const uint32_t TX_DESC_WITH_OOB     = (1<<30);
   const uint32_t TX_DESC_HAS_OWN_MAC  = (1<<28);

   `define RX_DESC_VALID(d) ((d) & (1<<31) ? 1 : 0)
   `define RX_DESC_ERROR(d) ((d) & (1<<30) ? 1 : 0)
   `define RX_DESC_HAS_OOB(d)  ((d) & (1<<29) ? 1 : 0)
   `define RX_DESC_SIZE(d)  (((d) & (1<<0) ? -1 : 0) + (d & 'hffe))
   
   protected CBusAccessor acc_regs, acc_pmem;
   protected uint32_t base_regs, base_pmem;
   protected int pmem_size;
   protected bit little_endian;

   protected uint32_t tx_head, tx_base, tx_avail, tx_size, tx_count, tx_oob_val;
   protected uint32_t rx_head, rx_base, rx_avail, rx_size, rx_count;
   

   protected EthPacket rx_queue[$];
   protected EthPacket tx_queue[$];

   const int MINIC_MTU  = 1536;
   
   function new(int pmem_size_, CBusAccessor regs_, uint32_t base_regs_, CBusAccessor pmem_, uint32_t base_pmem_);
      base_pmem      = base_pmem_;
      base_regs      = base_regs_;
      acc_regs       = regs_;
      acc_pmem       = pmem_;
      pmem_size      = pmem_size_;
      little_endian  = 1;

    
      
   endfunction // new

   task minic_writel(uint32_t addr, uint32_t val);
      acc_regs.write(base_regs + addr, val, 4);
   endtask // minic_writel

   task minic_readl(uint32_t addr, output uint32_t val);
      uint64_t tmp;
      acc_regs.read(base_regs + addr, tmp, 4);
      val  = tmp;
      
   endtask // minic_writel
   
   task new_tx_buffer();
      tx_head = tx_base;
      tx_avail = (tx_size - MINIC_MTU) >> 2;
      minic_writel(`ADDR_MINIC_TX_ADDR, tx_base);
   endtask // new_tx_buffers

   task new_rx_buffer();
      rx_head = rx_base;

      minic_writel(`ADDR_MINIC_MCR, 0);
      minic_writel(`ADDR_MINIC_RX_ADDR, rx_base);
      minic_writel(`ADDR_MINIC_RX_SIZE, rx_size >> 2);
      minic_writel(`ADDR_MINIC_EIC_ISR, `MINIC_EIC_ISR_RX);
      minic_writel(`ADDR_MINIC_MCR, `MINIC_MCR_RX_EN);
   endtask // new_rx_buffer
   

   task init();
      uint32_t lo, hi;
      
      minic_writel(`ADDR_MINIC_EIC_IDR, `MINIC_EIC_IDR_RX);
      minic_writel(`ADDR_MINIC_EIC_ISR, `MINIC_EIC_ISR_RX);

      rx_base     = base_pmem;
      rx_size     = pmem_size / 4;
      tx_base     = base_pmem + pmem_size / 2;
      tx_size     = pmem_size / 4;
      tx_oob_val  = 12345;


      lo = rx_base >> 2;
      hi = (rx_base >> 2) + (rx_size >> 2) - 1;

      minic_writel(`ADDR_MINIC_MPROT, (lo << `MINIC_MPROT_LO_OFFSET) | (hi << `MINIC_MPROT_HI_OFFSET));
      
      tx_count    = 0;
      rx_count    = 0;
      
      new_rx_buffer();
      minic_writel(`ADDR_MINIC_EIC_IER, `MINIC_EIC_IER_RX);
   endtask // init

   task tx_frame(byte payload[], uint32_t size, bit with_oob, int ts_id, output uint32_t ts, output int port_id);
      int i;
      uint32_t d_hdr, mcr, nwords;
      u64_array_t buff;
      byte tmp[];
      byte oob[2];
      
      new_tx_buffer();

      
      if(size < 60) size  = 60;
      if(size & 1) size  = size + 1;

      tmp = new[size](payload);
    
      buff = SimUtils.pack({0,0,0,0, tmp, 0,0,0,0}, 4, 1);
      size = size / 2;
      
      for(i=0;i<buff.size(); i++)
        acc_pmem.write(tx_head + i*4, buff[i], 4);
      
      acc_pmem.write(tx_head, TX_DESC_HAS_OWN_MAC | TX_DESC_VALID | (with_oob ? TX_DESC_WITH_OOB :0 )| size | (ts_id << 12), 4);
      
      minic_readl(`ADDR_MINIC_MCR, mcr);
      minic_writel(`ADDR_MINIC_MCR, mcr | `MINIC_MCR_TX_START);
   endtask // tx_frame


   task rx_frame(ref byte payload[], output uint32_t size, output bit with_ts, output uint32_t ts);
      uint32_t payload_size, num_words;
      uint64_t desc_hdr;
      uint32_t raw_ts;
      uint32_t rx_addr_cur, mcr, cur_avail;
      u64_array_t pbuff;
      
      int i;
      int n_recvd;
      uint32_t isr;
      
      minic_readl(`ADDR_MINIC_EIC_ISR, isr);
      
      if(! (isr & `MINIC_EIC_ISR_RX))
        return;

      acc_pmem.read(rx_head, desc_hdr);
      
      if(!`RX_DESC_VALID(desc_hdr))
        begin
           $error("SimDRV_Minic::rx_frame: weird, invalid RX desc header");
           $stop;
        end

      payload_size    = `RX_DESC_SIZE(desc_hdr);
      num_words       = (payload_size + 3) >> 2;
      pbuff           = new [num_words];

      //   $display("NWords %d hdr %x", num_words, desc_hdr);
      
      

      if(`RX_DESC_HAS_OOB(desc_hdr))
        payload_size  = payload_size - 6;
      
      
      if(!`RX_DESC_ERROR(desc_hdr))
        begin
           for(i=0; i<num_words;i++)
             acc_pmem.read((rx_head + 4 + i * 4) % rx_size, pbuff[i]);

           payload  = SimUtils.unpack(pbuff, 4, payload_size);
        end
      size          = payload_size;
      

      rx_head = (rx_head + 4 + num_words * 4) % rx_size;
      minic_writel(`ADDR_MINIC_RX_AVAIL, (num_words + 1));

      minic_readl(`ADDR_MINIC_RX_AVAIL, cur_avail);

      acc_pmem.read(rx_head, desc_hdr);

      if( cur_avail == (rx_size>>2) || !(`RX_DESC_VALID(desc_hdr)))
        begin
           minic_readl(`ADDR_MINIC_MCR, mcr);
           
           if(mcr & `MINIC_MCR_RX_FULL)
             new_rx_buffer();

           minic_writel(`ADDR_MINIC_EIC_ISR, `MINIC_EIC_ISR_RX);
        end

      
   endtask // rx_frame
   
   task do_rx();
      byte payload[];
      uint32_t size, ts;
      bit with_ts;

      rx_frame(payload, size, with_ts, ts);

      if(payload.size() > 0)
        begin
           EthPacket pkt;
           pkt  = new;
           pkt.deserialize(payload);
           rx_queue.push_back(pkt);
        end
      
   endtask // do_rx
   
     
   
   task run();
      uint32_t mcr;
      
      if(tx_queue.size() > 0)
        begin
           minic_readl(`ADDR_MINIC_MCR, mcr);
        //   $display("mcr %x, Minic::q_not_empty %d", mcr, tx_queue.size());
          
           if(mcr & `MINIC_MCR_TX_IDLE) begin
              byte b[];
              uint32_t ts;
              int pid;
              EthPacket pkt;

              pkt  = tx_queue.pop_front();
              pkt.serialize(b);
              tx_frame(b, b.size(), pkt.oob_type == TX_FID ? 1 : 0, pkt.ts.frame_id,ts, pid);
              
           end
        end // if (tx_queue.size() > 0)

      do_rx();
      
      
   endtask // run

   task send(EthPacket pkt);
      tx_queue.push_back(pkt);
   endtask // send

   function poll();
      return rx_queue.size() > 0;
   endfunction // poll

   task recv(ref EthPacket pkt);
      pkt  = rx_queue.pop_front();
   endtask // recv
   
   
   
   
endclass // CSimDrv_Minic
