`timescale 1ns/1ps

`include "gn4124_bfm.svh"   

const uint64_t BASE_WRPC = 'h0080000;

module main;
   reg clk_125m_pllref = 0;
   reg clk_20m_vcxo = 0;
   
   always #4ns clk_125m_pllref <= ~clk_125m_pllref;
   always #20ns clk_20m_vcxo <= ~clk_20m_vcxo;
   
   IGN4124PCIMaster I_Gennum ();

   spec_top
     DUT (
          .clk_125m_pllref_p_i(clk_125m_pllref),
          .clk_125m_pllref_n_i(~clk_125m_pllref),
          .clk_20m_vcxo_i(clk_20m_vcxo),

          `GENNUM_WIRE_SPEC_PINS(I_Gennum)
	  );

   initial begin
      uint64_t rval;
      
      CBusAccessor acc ;
      acc = I_Gennum.get_accessor();
      @(posedge I_Gennum.ready);

      #1us;
      $display("dupadupa");
      
      //acc.write(BASE_WRPC + 'h100, 'hdeadbeef);
      //acc.write(BASE_WRPC + 'h104, 'hcafebabe);

      //acc.read(BASE_WRPC + 'h100, rval);
      //$display("MemReadback1 %x", rval);
      //acc.read(BASE_WRPC + 'h104, rval);
      //$display("MemReadback2 %x", rval);

      //acc.write('ha0400, 'h1deadbee);
      //$display("dupa1");
      //acc.write('ha0400, 'h0deadbee);
      //$display("dupa2");
      acc.write('ha021c, 'hfafa);
      $display("dupa3");
      
      
   end
   
   
endmodule // main



