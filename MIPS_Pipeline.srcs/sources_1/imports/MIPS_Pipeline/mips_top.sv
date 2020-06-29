module mips_top(input  logic       CLK100MHZ, BTND,
                input  logic       BTNL, BTNR,
                input  logic [15:0] SW,
                output logic [11:0] LED,
                output logic  [7:0] AN,
                output logic        DP,
                output logic  [6:0] A2G);

  logic [31:0] pc, instr, readdata, dataadr, writedata, result;
  logic        memwrite;
  logic [26:0] clkdiv;
  logic        reset;
  
  assign reset = BTND;
  clkd clkd(CLK100MHZ, reset, clkdiv);
  mips mips(clkdiv[0], reset, pc, instr, memwrite, dataadr, 
            writedata, result, readdata);
  imem imem(pc[7:2], instr);
  dmem_with_io dmem(clkdiv[0], memwrite, dataadr, writedata, readdata,
            reset, BTNL, BTNR, SW, LED);
  disp disp(clkdiv[18:16], reset, {SW, 4'b0, LED}, AN, DP, A2G); 
            // 380KHz divide freq, choosing next digital tube every 2.6ms
endmodule
