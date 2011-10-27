`ifndef __WB_PACKET_SINK_SVH
 `define __WB_PACKET_SINK_SVH

`include "simdrv_defs.svh"
`include "eth_packet.svh"
`include "if_wishbone_accessor.svh"

`include "wb_fabric_defs.svh"

virtual class EthPacketSink;

   static int _null  = 0;
   
   pure virtual function int poll();
   pure virtual task recv(ref EthPacket pkt, ref int result = _null);      
   
   endclass // EthPacketSink


class WBPacketSink extends EthPacketSink;
   protected CWishboneAccessor m_acc;

   
   function new(CWishboneAccessor acc);
      m_acc  = acc;
   endfunction // new

   function int poll();
      return m_acc.poll();
      endfunction // poll

   protected task decode_status(uint64_t stat, ref EthPacket pkt);
      if(stat & 'h2)
       pkt.error      = 1'b1;
     else begin
        pkt.has_smac  = (stat & 'h4 ? 1'b1 : 1'b0);
        pkt.has_crc   = (stat & 'h8 ? 1'b1 : 1'b0);
        pkt.pclass    = (stat>>8) & 'hff;
        
     end
   endtask // decode_status
   
   protected task decode_oob(uint64_t oob, int size, ref EthPacket pkt);
      if(!size)
        return;
      else if(size == 2 && (oob >> 28) == WRF_OOB_TX_FID)
        begin
         //  $display("GotTxOOB");
           pkt.oob_type     = TX_FID;
           pkt.ts.frame_id  = oob & 'hffff;
        end 
      else if (size == 3 && (oob >> 46) == WRF_OOB_RX_TIMESTAMP)
        begin
           $display("GotRXOOB");
           
        end else begin
           $error("Invalid OOB!");
           $stop;
        end
      
      
   endtask // decode_oob
   
   
   task recv(ref EthPacket pkt, ref int result = _null);
      uint64_t oob = 0;
      byte tmp[];
      wb_cycle_t cyc;
      int i, size  = 0, n = 0, n_oob = 0;
      int oob_size = 0;
      

      
      pkt          = new;
      m_acc.get(cyc);

      
      for(i=0;i<cyc.data.size(); i++)
        if (cyc.data[i].a == WRF_DATA)
          size           = size + cyc.data[i].size;

      tmp                = new[size];

//      $display("CDS %d size: %d\n", cyc.data.size(), size);
      
      
      for(i=0;i<cyc.data.size(); i++)
        begin
           wb_xfer_t xf  = cyc.data[i];
           
           case(xf.a)
             WRF_STATUS:
               begin
                  decode_status(xf.d, pkt);
                  if(pkt.error)
                    break;
               end

             WRF_DATA:
               begin
                  if(xf.size == 1)
                    tmp[n++]  = (xf.d  & 'hff);
                  else if(xf.size == 2)begin
                     tmp[n++]  = ((xf.d >> 8)  & 'hff);
                     tmp[n++]  = (xf.d  & 'hff);
                  end
               end

             WRF_OOB:
               begin
                  oob       = (oob << 16) | xf.d;
                  oob_size ++;
               end
             
             endcase // case (xf.a)
        end
      pkt.deserialize(tmp);
      decode_oob(oob, oob_size, pkt);
      
      

endtask // recv
   
   

endclass // WBPacketSink



`endif
