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
            output logic [31:0] pc_f,
            input  logic [31:0] instr_f,
            output logic        memwrite_m,
            output logic [31:0] aluout_m, writedata_m, result_w,
            input  logic [31:0] readdata_m);

  logic [31:0] instr_d;
  logic        memtoreg_w, alusrc_e, aluext_e, regdst_e, memtoreg_e,
               regwrite_m, regwrite_w, jump, pcsrc_d, zero, flush_e;
  logic [2:0]  alucontrol_e;

  controller c(clk, reset, instr_d[31:26], instr_d[5:0], zero, flush_e,
               memtoreg_w, memwrite_m, pcsrc_d,
               alusrc_e, aluext_e, regdst_e,
               memtoreg_e, regwrite_m, regwrite_w, jump,
               alucontrol_e);
  datapath dp(clk, reset, memtoreg_w, pcsrc_d,
              alusrc_e, aluext_e, regdst_e,
              memtoreg_e, regwrite_m, regwrite_w, jump,
              alucontrol_e,
              zero, flush_e,
              pc_f, instr_f,
              aluout_m, writedata_m, result_w, instr_d,
              readdata_m);
endmodule

module controller(input  logic       clk, reset,
                  input  logic [5:0] op, funct,
                  input  logic       zero, flush_e,
                  output logic       memtoreg_w, memwrite_m,
                  output logic       pcsrc_d, alusrc_e, aluext_e,
                  output logic       regdst_e,
                  output logic       memtoreg_e, regwrite_m, regwrite_w,
                  output logic       jump,
                  output logic [2:0] alucontrol_e);
                  
  logic       jump;
  logic       pcsrc_d;
  logic       branch_d;
  logic       branchcond;  // Branch Condition Satisfied? (0 or 1)
    // Currently asserted to 0. Doesn't support J, BEQ or BNE in this version!!!
  logic       zero;

  logic        regwrite_d,   regwrite_e,   regwrite_m, regwrite_w;
  logic        memtoreg_d,   memtoreg_e,   memtoreg_m, memtoreg_w;
  logic        memwrite_d,   memwrite_e,   memwrite_m;
  logic        branchctrl_d;                                      // Branch Control: beq or bne?
  logic [2:0]  alucontrol_d, alucontrol_e;
  logic        alusrc_d,     alusrc_e;
  logic        aluext_d,     aluext_e;
  logic        regdst_d,     regdst_e;
  
  logic [2:0]  aluop;  // Caution all bandwidths when modifying one presence.


  maindec md(op, memtoreg_d, memwrite_d, branch_d, branchctrl_d,
             alusrc_d, aluext_d, regdst_d, regwrite_d, jump, aluop);
  aludec  ad(funct, aluop, alucontrol_d);
  mux2 #(1)  branchmux(zero, ~zero, branchctrl_d, branchcond); // Unsupported!!!
  assign pcsrc_d = branch_d & branchcond;
  
  floprc #(1) decode2execute_1(clk, reset, flush_e,   regwrite_d,   regwrite_e);
  floprc #(1) decode2execute_2(clk, reset, flush_e,   memtoreg_d,   memtoreg_e);
  floprc #(1) decode2execute_3(clk, reset, flush_e,   memwrite_d,   memwrite_e);
  floprc #(3) decode2execute_4(clk, reset, flush_e,   alucontrol_d, alucontrol_e);
  floprc #(1) decode2execute_5(clk, reset, flush_e,   alusrc_d,     alusrc_e);
  floprc #(1) decode2execute_6(clk, reset, flush_e,   aluext_d,     aluext_e);
  floprc #(1) decode2execute_7(clk, reset, flush_e,   regdst_d,     regdst_e);
  
  flopr #(1) execute2memory_1(clk, reset,   regwrite_e,   regwrite_m); 
  flopr #(1) execute2memory_2(clk, reset,   memtoreg_e,   memtoreg_m);
  flopr #(1) execute2memory_3(clk, reset,   memwrite_e,   memwrite_m);
  
  flopr #(1) memory2writeback_1(clk, reset, regwrite_m,   regwrite_w);
  flopr #(1) memory2writeback_2(clk, reset, memtoreg_m,   memtoreg_w);
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
                input  logic        memtoreg_w, pcsrc_d,
                input  logic        alusrc_e, aluext_e, regdst_e,
                input  logic        memtoreg_e, regwrite_m, regwrite_w, jump,
                input  logic [2:0]  alucontrol_e,
                output logic        zero, flush_e,
                output logic [31:0] pc_f,
                input  logic [31:0] instr_f,
                output logic [31:0] aluout_m, writedata_m, result_w, instr_d,
                input  logic [31:0] readdata_m);

  logic [31:0] pcnextbr,  pcbranch_d; // Unsupported!!!
  logic [31:0] pcnext;
  logic [31:0] pc_f;
  logic        stall_f;
  logic [31:0] instr_f,   instr_d;
  logic [31:0] pcplus4_f, pcplus4_d;
  logic                   stall_d;
  logic [31:0]            rfread1_d,   rfread1_e;
  logic [31:0]            rfread2_d,   rfread2_e;
  logic                                flush_e;
  logic [31:0]                         srca_e;
  logic [31:0]                         srcb_e;
  logic [4:0]             rs_d,        rs_e;
  logic [4:0]             rt_d,        rt_e;
  logic [4:0]             rd_d,        rd_e;
  logic [31:0]            zeroimm_d,   zeroimm_e;
  logic [31:0]            signimm_d,   signimm_e;
  logic [31:0]            signimmsh_d;
  logic [31:0]                         extimm_e;
  logic [1:0]                          forwarda_e;
  logic [1:0]                          forwardb_e;
  logic [4:0]                          writereg_e,  writereg_m,  writereg_w;
  logic [31:0]                         aluout_e,    aluout_m,    aluout_w;
  logic [31:0]                         writedata_e, writedata_m;
  logic [31:0]                                                   result_w;
  logic [31:0]                                      readdata_m,  readdata_w;
    
  // next PC logic
  flopenr #(32) pcreg(clk, reset, ~stall_f, pcnext, pc_f);
  adder       pcadd1_f(pc_f, 32'b100, pcplus4_f);
  sl2         immsh_d(signimm_d, signimmsh_d);
  adder       pcadd2_d(pcplus4_d, signimmsh_d, pcbranch_d);
  mux2 #(32)  pcbrmux(pcplus4_f, pcbranch_d, pcsrc_d, pcnextbr);
  mux2 #(32)  pcmux(pcnextbr, {pcplus4_d[31:28], 
                    instr_d[25:0], 2'b00}, jump, pcnext);

  // register file logic
  assign rs_d = instr_d[25:21];
  assign rt_d = instr_d[20:16];
  assign rd_d = instr_d[15:11];
  regfile     rf(clk, reset, regwrite_w, rs_d, rt_d, 
                 writereg_w, result_w, rfread1_d, rfread2_d);//srca_d, writedata_d);
  mux2 #(5)   wrmux_e(rt_e, rd_e, regdst_e, writereg_e);
  mux2 #(32)  resmux_w(aluout_w, readdata_w, memtoreg_w, result_w);
  signext     se_d(instr_d[15:0], signimm_d);
  zeroext     ze_d(instr_d[15:0], zeroimm_d);

  // ALU logic
  mux3 #(32)  fwamux_e(rfread1_e, result_w, aluout_m, forwarda_e, srca_e);
  mux3 #(32)  fwbmux_e(rfread2_e, result_w, aluout_m, forwardb_e, writedata_e);
  mux2 #(32)  extmux_e(signimm_e, zeroimm_e, aluext_e, extimm_e);
  mux2 #(32)  srcbmux_e(writedata_e, extimm_e, alusrc_e, srcb_e);
  alu         alu(srca_e, srcb_e, alucontrol_e, aluout_e, zero);
  
  // hazard unit
  hazardunit  hz(memtoreg_e, regwrite_m, regwrite_w, rs_d, rt_d, rs_e, rt_e, writereg_m, writereg_w,
                 forwarda_e, forwardb_e, stall_d, stall_f, flush_e);
  
  // pipeline registers
  flopenr #(32) fetch2decode_11(clk, reset, ~stall_d, instr_f,     instr_d);
  flopenr #(32) fetch2decode_12(clk, reset, ~stall_d, pcplus4_f,   pcplus4_d);
  
  floprc #(32) decode2execute_11(clk, reset, flush_e,   rfread1_d,   rfread1_e);
  floprc #(32) decode2execute_12(clk, reset, flush_e,   rfread2_d,   rfread2_e);
  floprc #(5)  decode2execute_13(clk, reset, flush_e,   rs_d,        rs_e);
  floprc #(5)  decode2execute_14(clk, reset, flush_e,   rt_d,        rt_e);
  floprc #(5)  decode2execute_15(clk, reset, flush_e,   rd_d,        rd_e);
  floprc #(32) decode2execute_16(clk, reset, flush_e,   signimm_d,   signimm_e);
  floprc #(32) decode2execute_17(clk, reset, flush_e,   zeroimm_d,   zeroimm_e);
  
  flopr #(32) execute2memory_11(clk, reset,   aluout_e,    aluout_m);
  flopr #(32) execute2memory_12(clk, reset,   writedata_e, writedata_m);
  flopr #(5)  execute2memory_13(clk, reset,   writereg_e,  writereg_m);
  
  flopr #(32) memory2writeback_11(clk, reset, readdata_m,  readdata_w);
  flopr #(32) memory2writeback_12(clk, reset, aluout_m,    aluout_w);
  flopr #(5)  memory2writeback_13(clk, reset, writereg_m,  writereg_w);
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

  always_ff @(negedge clk, posedge reset)
    if (reset)
      for (i = 0; i <= 31; i++) rf[i] <= 32'b0;
    else if (we3) rf[wa3] <= wd3;
  //always_ff @(posedge clk)
    //if (we3) rf[wa3] <= wd3;	

  assign rd1 = (ra1 != 0) ? rf[ra1] : 0;
  assign rd2 = (ra2 != 0) ? rf[ra2] : 0;
endmodule

module hazardunit(input  logic       memtoreg_e, regwrite_m, regwrite_w,
                  input  logic [4:0]  rs_d, rt_d, rs_e, rt_e, writereg_m, writereg_w,
                  output logic [1:0]  forwarda_e, forwardb_e,
                  output logic        stall_f, stall_d, flush_e);
  // forwarding
  always_comb
    if (rs_e != '0 && rs_e == writereg_m && regwrite_m) forwarda_e = 2'b10;
    else if (rs_e != '0 && rs_e == writereg_w && regwrite_w) forwarda_e = 2'b01;
    else forwarda_e = 2'b00;
  always_comb
    if (rt_e != '0 && rt_e == writereg_m && regwrite_m) forwardb_e = 2'b10;
    else if (rt_e != '0 && rt_e == writereg_w && regwrite_w) forwardb_e = 2'b01;
    else forwardb_e = 2'b00;
  
  // stalls
  logic lwstall;
  assign lwstall = ((rs_d == rt_e) | (rt_d == rt_e)) & memtoreg_e;
  assign stall_f = lwstall;
  assign stall_d = lwstall;
  assign flush_e = lwstall;
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

module flopenr #(parameter WIDTH = 8)
              (input  logic             clk, reset, en,
               input  logic [WIDTH-1:0] d, 
               output logic [WIDTH-1:0] q);

  always_ff @(posedge clk, posedge reset)
    if (reset) q <= 0;
    else if (en) q <= d;
endmodule

module floprc #(parameter WIDTH = 8)
              (input  logic             clk, reset, clear,
               input  logic [WIDTH-1:0] d, 
               output logic [WIDTH-1:0] q);

  always_ff @(posedge clk, posedge reset)
    if (reset)      q <= 0;
    else if (clear) q <= 0;
    else            q <= d;
endmodule

module mux2 #(parameter WIDTH = 8)
             (input  logic [WIDTH-1:0] d0, d1, 
              input  logic             s, 
              output logic [WIDTH-1:0] y);

  assign y = s ? d1 : d0; 
endmodule

module mux3 #(parameter WIDTH = 8)
             (input  logic [WIDTH-1:0] d0, d1, d2, 
              input  logic [1:0]       s, 
              output logic [WIDTH-1:0] y);

  always_comb
    if (s == 2'b10) y = d2;
    else if (s == 2'b01) y = d1;
    else if (s == 2'b00) y = d0;
    else y = 'bx;
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
