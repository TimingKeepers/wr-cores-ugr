`ifndef __WB_PACKET_SOURCE_SVH
 `define __WB_PACKET_SOURCE_SVH

`include "simdrv_defs.svh"
`include "eth_packet.svh"
`include "if_wishbone_accessor.svh"

`include "wb_fabric_defs.svh"



class WBPacketSource extends EthPacketSource;
   protected CWishboneAccessor m_acc;

    function new(CWishboneAccessor acc);
      m_acc  = acc;
   endfunction // new

   function bit[15:0] pack_status(ref EthPacket pkt, input bit error = 0);
      bit [15:0] st;
      st[0]  = (pkt.is_hp ? 1'b1: 1'b0);
      st[1]  = 1'b0;
      st[2]  = (pkt.has_smac ? 1'b1: 1'b0);
      st[3]  = error;
      st[15:8] = pkt.pclass; // FIXME: add packet classes
      st[7:4]= 0;
      
      return st;
   endfunction // pack_status

   task unpack_status(bit[15:0] status, ref EthPacket pkt);

   endtask // unpack_status

   typedef bit[15:0] oob_array16[];

   function u64_array_t pack_oob(ref EthPacket pkt);
      u64_array_t oob;

      case(pkt.oob_type)
        TX_FID: begin
           oob     = new[2];
           oob[0]  = {WRF_OOB_TX_FID, 12'b0};
           oob[1]  = pkt.ts.frame_id;
        end
      endcase // case (pkt.oob_type)
      return oob;
      
   endfunction // pack_oob
   
   
   task send(ref EthPacket pkt, ref int result = _null);
      byte pdata[]; // FIXME: dynamic allocation would be better...
      u64_array_t pdata_p;
      u64_array_t oob_p;
      
      int i, len;
      
      wb_cycle_t cyc;
      wb_xfer_t xf;
      
      cyc.ctype  = PIPELINED;
      cyc.rw     = 1;
      
      
      /* First, the status register */
      
      xf.a       = WRF_STATUS;
      xf.d       = pack_status(pkt);
      xf.size    = 2;

      cyc.data.push_back(xf); 
      
      pkt.serialize(pdata);
      
      pdata_p            = SimUtils.pack(pdata, 2, 1);
      len                = pdata_p.size();
      
      for(i=0; i < len; i++)
        begin
           xf.a          = WRF_DATA;
           if(i==len-1 && (pdata.size()&1))
             begin
                xf.size  = 1;
                xf.d     = pdata_p[i] >> 8;
             end else begin
                xf.size     = 2;
                xf.d     = pdata_p[i];
             end
           
           cyc.data.push_back(xf); 

           end

      if(pkt.error)
        begin

           xf.a     = WRF_STATUS;
           xf.d     = pack_status(pkt, 1);
           xf.size  = 2;
           cyc.data.push_back(xf); 

           
        end else begin
      
//      $display("WBPacketSource::send(): DataSize: %d\n", cyc.data.size());

           oob_p    = pack_oob(pkt);

      for (i=0;i<oob_p.size(); i++)
        begin
           xf.a     = WRF_OOB;
           xf.d     = oob_p[i] & 'hffff;
           xf.size  = 2;
           cyc.data.push_back(xf); 

        end
        end // else: !if(pkt.error)
      
      m_acc.put(cyc);
      m_acc.get(cyc);

      result  = cyc.result;
      
   endtask // send
   
      
   
      
      

endclass // WBPacketSource



`endif
