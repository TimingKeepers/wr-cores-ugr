/* Added by GSI to support debug over wishbone */

`define ACK_DELAY 8 /* Give the JTAG core time to latch after a write */

`include "lm32_include.v"

module jtag_wb (
    clk_i,
    DAT_I,
    ADR_I,
    CYC_I,
    SEL_I,
    STB_I,
    WE_I,
    reg_d,
    reg_addr_d,
    ACK_O,
    STALL_O,
    DAT_O,
    reg_update,
    reg_q,
    reg_addr_q,
    jtck,
    jrstn
);

input clk_i;
input [`LM32_WORD_RNG] DAT_I;
input [`LM32_WORD_RNG] ADR_I;
input CYC_I;
input [`LM32_BYTE_SELECT_RNG] SEL_I;
input STB_I;
input WE_I;
input [7:0] reg_d;
input [2:0] reg_addr_d;

output ACK_O;
output STALL_O;
output [`LM32_WORD_RNG] DAT_O;
output reg_update;
output [7:0] reg_q;
output [2:0] reg_addr_q;
output jtck;
output jrstn;

reg [7:0] reg_q;
reg [2:0] reg_addr_q;
reg [`ACK_DELAY-1:0] ack_shift;

assign reg_update = (CYC_I == `TRUE) && 
                    (STB_I == `TRUE) &&
                    (WE_I == `TRUE);

assign DAT_O[31:11] = 21'h0;
assign DAT_O[10:3] = reg_d;
assign DAT_O[2:0] = reg_addr_d;

assign jtck = clk_i;
assign jrstn = 1;
assign ACK_O = ack_shift[0];
assign STALL_O = |ack_shift[`ACK_DELAY-1:1];

always @(posedge clk_i)
begin
    ack_shift <= 
      {CYC_I == `TRUE && STB_I == `TRUE && STALL_O == `FALSE,
		 ack_shift[`ACK_DELAY-1:1]};
    
    if (reg_update == `TRUE)
    begin
        reg_q <= DAT_I[10:3];
        reg_addr_q <= DAT_I[2:0];
    end
end

endmodule
