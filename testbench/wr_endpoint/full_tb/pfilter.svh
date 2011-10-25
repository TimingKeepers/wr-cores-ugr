/* Packet Filter microcode definitions */


class PFilterMicrocode;
   typedef enum 
    {
     AND = 0,
     NAND = 4,
     OR = 1,
     NOR = 5,
     XOR = 2,
     XNOR = 6,
     MOV = 3,
     NOT = 7
     } pfilter_op_t;


   const uint64_t PF_MODE_LOGIC    = (1<<34);
   const uint64_t PF_MODE_CMP    = 0;

   const int max_size         = 64;
   
   protected int code_pos;
   protected uint64_t code_buf[];

   function new();
      code_pos  = 0;
      code_buf  = new[max_size];
   endfunction // new

   task check_size();
      if(code_pos == max_size - 1)
        $error("microcode: code too big (max size: %d)", max_size);
   endtask // check_size

   task check_reg_range(int val, int minval, int maxval, string name);
      if(val < minval || val > maxval)
        $error("microcode: %s register out of range (%d to %d)", name, minval,maxval);
   endtask // check_reg_range
   
   // rd = (packet[offset] & mask == value) op rd
   task cmp(int offset, int value, int mask, pfilter_op_t op, int rd);
      uint64_t ir;

      check_size();
      
      if(offset > code_pos-1)
        $error("microcode: comparison offset is bigger than current PC. Insert some nops before comparing");

      check_reg_range(rd, 1, 15, "ra/rd");
   
      ir  = (PF_MODE_CMP | (offset << 7)
            | ((mask & 'h1) ? (1<<29) : 0)
            | ((mask & 'h10) ? (1<<30) : 0)
            | ((mask & 'h100) ? (1<<31) : 0)
            | ((mask & 'h1000) ? (1<<32) : 0))
        | op | (rd << 3);

      ir                    = ir | (value & 'hffff) << 13;

      code_buf[code_pos++]  = ir;
   endtask // cmp


   // rd                    = (packet[offset] & (1<<bit_index)) op rd
   task btst(int offset, int bit_index, pfilter_op_t op, int rd);
      uint64_t ir;

      check_size();
      
      if(offset > code_pos-1)
        $error("microcode: comparison offset is bigger than current PC. Insert some nops before comparing");

      check_reg_range(rd, 1, 15, "ra/rd");
      check_reg_range(bit_index, 0, 15, "bit index");
   
      ir                    = ((1<<33) | PF_MODE_CMP | (offset << 7) | (bit_index << 29) | op | (rd << 3));
      
      code_buf[code_pos++]  = ir;
   endtask // cmp
         
   task nop();
      uint64_t ir;
      check_size();
      ir  = PF_MODE_LOGIC;
      code_buf[code_pos++]  = ir;
   endtask // nop
         

   // rd  = ra op rb
   task logic2(int rd, int ra, pfilter_op_t op, int rb);
      uint64_t ir;
      check_size();
      check_reg_range(ra, 0, 31, "ra");
      check_reg_range(rb, 0, 31, "rb");
      check_reg_range(rd, 1, 31, "rd");

      ir  = (ra << 8) | (rb << 13) | ((rd & 'hf) << 3) | ((rd & 'h10) ? (1<<7) : 0) | op;
      ir  = ir | PF_MODE_LOGIC | (3<<23);
      code_buf[code_pos++]  = ir;
   endtask // logic2

   // rd  = (ra op rb) op2 rc
   task logic3(int rd, int ra, pfilter_op_t op, int rb, pfilter_op_t op2, int rc);
      uint64_t ir;
      check_size();
      check_reg_range(ra, 0, 31, "ra");
      check_reg_range(rb, 0, 31, "rb");
      check_reg_range(rc, 0, 31, "rb");
      check_reg_range(rd, 1, 31, "rd");

      ir  = (ra << 8) | (rb << 13) | (rc << 18) | ((rd & 'hf) << 3) | ((rd & 'h10) ? (1<<7) : 0) | op;
      ir  = ir | PF_MODE_LOGIC | (op2<<23);
      code_buf[code_pos++]  = ir;
   endtask // logic3

   typedef uint64_t u64_array[];
   
   
   function u64_array assemble();
      u64_array tmp;
      code_buf[code_pos++]  = (1<<35); // insert FIN instruction
      tmp                   = new [code_pos](code_buf);
      return tmp;
   endfunction // assemble
   
   
endclass // PFilterMicrocode
         
         
