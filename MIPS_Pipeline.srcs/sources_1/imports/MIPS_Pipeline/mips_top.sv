module mips_top(input  logic       clk, reset, 
                output logic [7:0] an,
                output logic       dp,
                output logic [6:0] a2g);

  logic [31:0] pc, instr, readdata, dataadr, writedata, result;
  logic        memwrite;
  logic [26:0] clkdiv;
  
  clkd clkd(clk, reset, clkdiv);
  mips mips(clkdiv[26], reset, pc, instr, memwrite, dataadr, 
            writedata, result, readdata);
  imem imem(pc[7:2], instr);
  dmem dmem(clkdiv[26], memwrite, dataadr, writedata, readdata);
  disp disp(clkdiv[18:16], reset, result, an, dp, a2g); 
            // 380KHz divide freq, choosing next digital tube every 2.6ms
endmodule
