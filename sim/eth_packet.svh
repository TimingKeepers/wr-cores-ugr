`ifndef __ETH_PACKET_SVH
`define __ETH_PACKET_SVH

`include "simdrv_defs.svh"

typedef byte mac_addr_t[6];
typedef bit[11:0] vid_t;
typedef bit[2:0] pcp_t;

typedef enum
  {
   NONE = 0,
   TX_FID,
   RX_TIMESTAMP
   } oob_type_t;

typedef struct {
   bit [27:0] ts_r;
   bit [3:0] ts_f;
   bit [5:0] port_id;
   bit [15:0] frame_id;
} wr_timestamp_t;

class EthPacket;

   static const mac_addr_t dummy_mac  = '{0,0,0,0,0,0}   ;

   static  int _zero             = 0;
   
   static const int CMP_OOB                  = 1;
   static const int CMP_STATUS               = 2;
   
   byte payload[];
   int size;

   mac_addr_t src, dst;
   oob_type_t oob_type;
   
   bit is_q;
   bit is_hp;  
   bit has_smac;
   bit has_crc;
   bit error;

   bit [15:0] ethertype;
   bit [7:0] pclass;
   
   vid_t vid;
   pcp_t pcp;
   
   oob_type_t oob;
   wr_timestamp_t ts;
   

   task set_size(int size);
      payload  = new[size](payload);
   endtask
   
   function new(int size = _zero);
      size      = 0;
      src       = dummy_mac;
      dst       = dummy_mac;
      is_q      = 0;
      is_hp     = 0;
      has_crc   = 0;
      oob_type  = NONE;
      payload  = new[size](payload);
      
   endfunction // new

   task deserialize(byte data[]);
      int i, hsize, psize;

      if(data.size < 14)
        begin
           error         = 1;
           return;
           end
      
      for(i=0; i<6;i++)
        begin
           dst[i]     = data[i];
           src[i]     = data [i+6];
        end

      has_crc         = 0;
      if(data[12] == 'h81 && data[13] == 'h00)
        begin
           is_q       = 1;
           hsize      = 18;
           ethertype  = {data[14], data[15]};
           vid        = ((int'(data[16]) << 8) | data[17]) & 12'hfff;
           pcp        = data[16] >> 5;
        end else begin
           is_q       = 0;
           hsize      = 14;
           ethertype  = {data[12], data[13]};
        end

      psize           = data.size() - hsize;

      if(psize <= 0)
      begin
         error    = 1;
         return;
      end
      
      payload       = new[psize];

      for(i=0;i<data.size() - hsize;i++)
        payload[i]  = data[hsize + i];

//      error           = 0;
   endtask
  
   task automatic serialize(ref byte data[]); 
      int i, hsize;

      hsize              = is_q ? 18 : 14;
      data               = new[payload.size() + hsize](data);
      
      for(i=0; i<6;i++)
        begin
           data[i]       = dst[i];
           data[i + 6]   = src[i];
        end

      
      if(is_q)
        begin
           data [12]     = 8'h81;
           data [13]     = 8'h00;
           data [14]     = ethertype[15:8];
           data [15]     = ethertype[7:0];
           data [16]     = {pcp, 1'b0, vid[11:8]};
           data [17]     = vid[7:0];
        end else begin
           data[12]      = ethertype [15:8];
           data[13]      = ethertype [7:0];
        end
      
      for (i=0; i<payload.size(); i++)
        data[i + hsize]  = payload[i];
   endtask // serialize

   function bit equal(ref EthPacket b, input int flags = 0);
      
      if(src != b.src || dst != b.dst || ethertype != b.ethertype)
        begin
           $display("notequal: hdr");
           return 0;
        end
      
      if(is_q ^ b.is_q)
        begin
           $display("notequal: q");
           return 0;
        end

      if(is_q && (vid != b.vid || pcp != b.pcp))
        return 0;

      if(payload != b.payload)
        begin
           $display("notequal: payload");
           return 0;
        end
//        return 0;

      
      if(flags & CMP_STATUS)
        if(error ^ b.error)
          return 0;

      if(flags & CMP_OOB) begin
         if (b.oob_type != oob_type)
          return 0;

         if(oob_type == TX_FID && (b.ts.frame_id != ts.frame_id))
           return 0;
      end 
      
      return 1;
   endfunction // equal
   

   task copy(ref EthPacket b);

      endtask // copy

   task hexdump(byte buffer []);
      string str;
      int size           ;
      int i;
      int offset         = 0;
      const int per_row  = 16;

      size               = buffer.size();

      
      while(size > 0)
        begin
           int n;
           n       = (size > per_row ? per_row : size);
           $sformat(str,"+%03x: ", offset);
           for(i=0;i<n;i++) $sformat(str,"%s%s%02x", str, (i==(per_row/2)?"-":" "), buffer[offset + i]);
           $display(str);

           offset  = offset + n;
           size    = size - n;
      end
      
      
   endtask // hexdump
   
   
   task dump(int full = _zero);
      string str, tmp;
      int t;
      
      if(is_q)
        $sformat(str, "802.1q [VID %5d/PCP %d] ", vid, pcp);
      else
        str  = "802.1                    ";

      $sformat(str, "%s DST [%02x:%02x:%02x:%02x:%02x:%02x] SRC: [%02x:%02x:%02x:%02x:%02x:%02x] Type = 0x%04x size = %d F:(%s%s)", str, dst[0],dst[1],dst[2],dst[3],dst[4],dst[5], src[0],src[1],src[2],src[3],src[4], src[5], ethertype, (is_q ? 18 : 14) + payload.size(), 
is_hp ? "H" : " ", has_crc ? "C" : " ");

      if(oob_type == TX_FID)
        begin
           $sformat(tmp, "TxOOB: %x", ts.frame_id);
           str  = {str, tmp};
        end

      $display(str);
      hexdump(payload);
   endtask // dump
   
      
endclass // EthPacket


class EthPacketGenerator;

   protected EthPacket template;
   protected int min_size, max_size;
   protected int seed;
   
   static const int SMAC              = (1<<0);
   static const int      DMAC         = (1<<1);
   static const int      ETHERTYPE    = (1<<2);
   static const int      VID          = (1<<3);
   static const int      PCP          = (1<<4); 
   static const int      PAYLOAD      = (1<<5);
   static const int      SEQ_PAYLOAD  = (1<<7);
   static const int      SEQ_ID  = (1<<10);
   static const int TX_OOB             = (1<<6);
   static const int EVEN_LENGTH        = (1<<8);
   static const int RX_OOB             = (1<<9);
   static const int      ALL          = SMAC | DMAC | VID | ETHERTYPE | PCP | PAYLOAD ;
   

   protected int r_flags;
   protected int m_current_frame_id;
   protected int cur_seq_id;

   function new();
      r_flags             =ALL;
      min_size            = 64;
      max_size            = 128;
      m_current_frame_id  = 0;
      template            = new;
      cur_seq_id = 0;
      
   endfunction // new
   
   task set_randomization(int flags);
      r_flags  = flags;
   endtask // randomize


   typedef byte dyn_array[];
   
   
   protected function dyn_array random_bvec(int size);
      byte v[];
      int i;
//      $display("RandomBVEC %d", size);
      
      v       = new[size](v);
      for(i=0;i<size;i++)
        v[i]  = $dist_uniform(seed, 0, 256);
   
      return v;
      
   endfunction // random_bvec

   task set_seed(int seed_);
      seed = seed_;
   endtask // set_seed

   function int get_seed();
      return seed;
   endfunction // get_seed
   
      
      
   
   protected function dyn_array seq_payload(int size);
      byte v[];
      int i;
      
      v       = new[size](v);
      for(i=0;i<size;i++)
        v[i]  = i;
   
      return v;
      
   endfunction // random_bvec
   
   
   function automatic EthPacket gen();
      EthPacket pkt;
      int len;
      

      pkt                                         = new;
      
      
      if (r_flags & SMAC) pkt.src                 = random_bvec(6); else pkt.src = template.src;
      if (r_flags & DMAC) pkt.dst                 = random_bvec(6); else pkt.dst =  template.dst;

      pkt.ethertype                               = (r_flags & ETHERTYPE ? $dist_uniform(seed, 0, 1<<16) : template.ethertype);
      pkt.is_q                                    = template.is_q;
      pkt.vid                                     = template.vid;
      pkt.pcp                                     = template.pcp;
      pkt.has_smac                                = template.has_smac;
      

//      $display("Size min %d max %d", min_size, max_size);

      len                                         = $dist_uniform(seed, min_size, max_size);

      if((len & 1) && (r_flags & EVEN_LENGTH))
        len++;
      
      
      if(r_flags & PAYLOAD) pkt.payload           = random_bvec(len);
      else if(r_flags & SEQ_PAYLOAD) pkt.payload  = seq_payload(len);
      else pkt.payload                            = template.payload;

      if(r_flags & SEQ_ID)
        begin
           pkt.payload[0] = cur_seq_id & 'hff;
           pkt.payload[1] = (cur_seq_id>>8) & 'hff;
           pkt.payload[2] = (cur_seq_id>>16) & 'hff;
           pkt.payload[3] = (cur_seq_id>>24) & 'hff;
           cur_seq_id++;
        end
      
      if(r_flags & TX_OOB)
        begin
           pkt.ts.frame_id                        = m_current_frame_id++;
           pkt.oob_type                           = TX_FID;
        end
      
      return pkt;
      
      
   endfunction
   
   task set_template(EthPacket pkt);
      template  = pkt;
   endtask // set_template
                
   task set_size(int smin, int smax);
      min_size  = smin;
      max_size  = smax;
   endtask // set_size

   

endclass // EthPacketGenerator



  

`endif
  