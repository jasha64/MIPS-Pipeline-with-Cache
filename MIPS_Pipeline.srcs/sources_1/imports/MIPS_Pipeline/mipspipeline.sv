// mips.sv
// From Section 7.6 of Digital Design & Computer Architecture
// Updated to SystemVerilog 26 July 2011 David_Harris@hmc.edu

module dmem(input  logic        clk, we,
            input  logic [31:0] a, wd,
            output logic [31:0] rd);

  logic [31:0] RAM[63:0];

  assign rd = RAM[a[31:2]]; // word aligned

  always_ff @(posedge clk)
    if (we) RAM[a[31:2]] <= wd;
endmodule

module imem(input  logic [5:0] a,
            output logic [31:0] rd);

  logic [31:0] RAM[80:0];

  initial begin
      $readmemh("../Obj/memfile.dat",RAM);
  end


  assign rd = RAM[a]; // word aligned
endmodule

module mips(input  logic        clk, reset,
            output logic [31:0] pc,
            input  logic [31:0] instr,
            output logic        memwrite,
            output logic [31:0] aluout, writedata, result,
            input  logic [31:0] readdata);

  logic       memtoreg, alusrc, aluext, regdst, 
              regwrite, jump, pcsrc, zero;
  logic [2:0] alucontrol;

  controller c(instr[31:26], instr[5:0], zero,
               memtoreg, memwrite, pcsrc,
               alusrc, aluext, regdst, regwrite, jump,
               alucontrol);
  datapath dp(clk, reset, memtoreg, pcsrc,
              alusrc, aluext, regdst, regwrite, jump,
              alucontrol,
              zero, pc, instr,
              aluout, writedata, result,
              readdata);
endmodule

module controller(input  logic [5:0] op, funct,
                  input  logic       zero,
                  output logic       memtoreg, memwrite,
                  output logic       pcsrc, alusrc, aluext,
                  output logic       regdst, regwrite,
                  output logic       jump,
                  output logic [2:0] alucontrol);

  logic [2:0] aluop;  // Caution all bandwidths when modifying one presence.
  logic       branch;  // Branch Enabled
  logic       branchctrl;  // Branch Control: beq or bne?
  logic       branchcond;  // Branch Condition Satisfied? (0 or 1)

  maindec md(op, memtoreg, memwrite, branch, branchctrl,
             alusrc, aluext, regdst, regwrite, jump, aluop);
  aludec  ad(funct, aluop, alucontrol);

  mux2 #(1)  branchmux(zero, ~zero, branchctrl, branchcond);
  assign pcsrc = branch & branchcond;
endmodule

module maindec(input  logic [5:0] op,
               output logic       memtoreg, memwrite,
               output logic       branch, branchctrl,
               output logic       alusrc, aluext,
               output logic       regdst, regwrite,
               output logic       jump,
               output logic [2:0] aluop);

  logic [11:0] controls;

  assign {
                                 regwrite,
                                   regdst,
                                     alusrc,
                                       aluext,
                                         branch,
                                           branchctrl,
                                             memwrite,
                                               memtoreg,
                                                 jump,
                                                   aluop
  } = controls;
  always_comb
    case(op)
      6'b000000: controls <= 12'b1_1_0_0_0_0_0_0_0_110; // RTYPE
      6'b100011: controls <= 12'b1_0_1_0_0_0_0_1_0_000; // LW
      6'b101011: controls <= 12'b0_0_1_0_0_0_1_0_0_000; // SW
      6'b000100: controls <= 12'b0_0_0_0_1_0_0_0_0_001; // BEQ
      6'b000101: controls <= 12'b0_0_0_0_1_1_0_0_0_001; // BNE
      6'b001000: controls <= 12'b1_0_1_0_0_0_0_0_0_000; // ADDI
      6'b001100: controls <= 12'b1_0_1_1_0_0_0_0_0_010; // ANDI
      6'b001101: controls <= 12'b1_0_1_1_0_0_0_0_0_011; // ORI
      6'b001010: controls <= 12'b1_0_1_1_0_0_0_0_0_100; // SLTI
      6'b000010: controls <= 12'b0_0_0_0_0_0_0_0_1_000; // J
      default:   controls <= 12'bx_x_x_x_x_x_x_x_x_xxx; // illegal op
    endcase
endmodule

module aludec(input  logic [5:0] funct,
              input  logic [2:0] aluop,
              output logic [2:0] alucontrol);

  always_comb
    case(aluop)
      3'b000: alucontrol <= 3'b010;  // add (for lw/sw/addi)
      3'b001: alucontrol <= 3'b110;  // sub (for beq)
      3'b010: alucontrol <= 3'b000;  // and (for andi)
      3'b011: alucontrol <= 3'b001;  // or  (for ori)
      3'b100: alucontrol <= 3'b111;  // slt (for slti)
      default: case(funct)          // R-type instructions
          6'b100000: alucontrol <= 3'b010; // add
          6'b100010: alucontrol <= 3'b110; // sub
          6'b100100: alucontrol <= 3'b000; // and
          6'b100101: alucontrol <= 3'b001; // or
          6'b101010: alucontrol <= 3'b111; // slt
          default:   alucontrol <= 3'bxxx; // ???
        endcase
    endcase
endmodule

module datapath(input  logic        clk, reset,
                input  logic        memtoreg, pcsrc,
                input  logic        alusrc, aluext, regdst,
                input  logic        regwrite, jump,
                input  logic [2:0]  alucontrol,
                output logic        zero,
                output logic [31:0] pc,
                input  logic [31:0] instr,
                output logic [31:0] aluout, writedata, result,
                input  logic [31:0] readdata);

  logic [4:0]  writereg;
  logic [31:0] pcnext, pcnextbr, pcplus4, pcbranch;
  logic [31:0] zeroimm, signimm, extimm, signimmsh;
  logic [31:0] srca, srcb;

  // next PC logic
  flopr #(32) pcreg(clk, reset, pcnext, pc);
  adder       pcadd1(pc, 32'b100, pcplus4);
  sl2         immsh(signimm, signimmsh);
  adder       pcadd2(pcplus4, signimmsh, pcbranch);
  mux2 #(32)  pcbrmux(pcplus4, pcbranch, pcsrc, pcnextbr);
  mux2 #(32)  pcmux(pcnextbr, {pcplus4[31:28], 
                    instr[25:0], 2'b00}, jump, pcnext);

  // register file logic
  regfile     rf(clk, reset, regwrite, instr[25:21], instr[20:16], 
                 writereg, result, srca, writedata);
  mux2 #(5)   wrmux(instr[20:16], instr[15:11],
                    regdst, writereg);
  mux2 #(32)  resmux(aluout, readdata, memtoreg, result);
  signext     se(instr[15:0], signimm);
  zeroext     ze(instr[15:0], zeroimm);

  // ALU logic
  mux2 #(32)  extmux(signimm, zeroimm, aluext, extimm);
  mux2 #(32)  srcbmux(writedata, extimm, alusrc, srcb);
  alu         alu(srca, srcb, alucontrol, aluout, zero);
endmodule

module regfile(input  logic        clk, reset,
               input  logic        we3, 
               input  logic [4:0]  ra1, ra2, wa3, 
               input  logic [31:0] wd3, 
               output logic [31:0] rd1, rd2);

  logic [31:0] rf[31:0];
  integer i;  // µ÷ÊÔ¹¦ÄÜ???

  // three ported register file
  // read two ports combinationally
  // write third port on rising edge of clk
  // register 0 hardwired to 0
  // note: for pipelined processor, write third port
  // on falling edge of clk

  always_ff @(posedge clk, posedge reset)
    if (reset)
      for (i = 0; i <= 31; i++) rf[i] <= 32'b0;
    else if (we3) rf[wa3] <= wd3;
  //always_ff @(posedge clk)
    //if (we3) rf[wa3] <= wd3;	

  assign rd1 = (ra1 != 0) ? rf[ra1] : 0;
  assign rd2 = (ra2 != 0) ? rf[ra2] : 0;
endmodule

module adder(input  logic [31:0] a, b,
             output logic [31:0] y);

  assign y = a + b;
endmodule

module sl2(input  logic [31:0] a,
           output logic [31:0] y);

  // shift left by 2
  assign y = {a[29:0], 2'b00};
endmodule

module signext(input  logic [15:0] a,
               output logic [31:0] y);
              
  assign y = {{16{a[15]}}, a};
endmodule

module zeroext(input  logic [15:0] a,
               output logic [31:0] y);
              
  assign y = {16'b0, a};
endmodule

module flopr #(parameter WIDTH = 8)
              (input  logic             clk, reset,
               input  logic [WIDTH-1:0] d, 
               output logic [WIDTH-1:0] q);

  always_ff @(posedge clk, posedge reset)
    if (reset) q <= 0;
    else       q <= d;
endmodule

module mux2 #(parameter WIDTH = 8)
             (input  logic [WIDTH-1:0] d0, d1, 
              input  logic             s, 
              output logic [WIDTH-1:0] y);

  assign y = s ? d1 : d0; 
endmodule

module alu(input  logic [31:0] a, b,
           input  logic [2:0]  alucontrol,
           output logic [31:0] result,
           output logic        zero);

  logic [31:0] condinvb, sum;

  assign condinvb = alucontrol[2] ? ~b : b;
  assign sum = a + condinvb + alucontrol[2];

  always_comb
    case (alucontrol[1:0])
      2'b00: result = a & b;
      2'b01: result = a | b;
      2'b10: result = sum;
      2'b11: result = sum[31];
    endcase

  assign zero = (result == 32'b0);
endmodule
