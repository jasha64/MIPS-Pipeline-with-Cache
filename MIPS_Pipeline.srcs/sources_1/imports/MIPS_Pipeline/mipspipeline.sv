module dmem(input  logic        clk, we,
            input  logic [31:0] a, wd,
            output logic [31:0] rd);

  logic [31:0] RAM[255:0];

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
  logic        branch_d, memtoreg_e, memtoreg_m, memtoreg_w, alusrca_e, alusrcb_e, aluext_e, regdst_e,
                regwrite_e, regwrite_m, regwrite_w, jump_d, jr_d, jal_w, pcsrc_d, flush_e, equal_d;
  logic [3:0]  alucontrol_e;

  controller c(clk, reset,
               instr_d[31:26], instr_d[5:0],
               flush_e, equal_d,
               memwrite_m, pcsrc_d, branch_d,
               alusrca_e, alusrcb_e,
               aluext_e, regdst_e,
               memtoreg_e, memtoreg_m, memtoreg_w,
               regwrite_e, regwrite_m, regwrite_w,
               jump_d, jr_d,
               jal_w,
               alucontrol_e);
  datapath dp(clk, reset,
              pcsrc_d, branch_d,
              alusrca_e, alusrcb_e,
              aluext_e, regdst_e,
              memtoreg_e, memtoreg_m, memtoreg_w,
              regwrite_e, regwrite_m, regwrite_w,
              jump_d, jr_d,
              jal_w,
              alucontrol_e,
              flush_e, equal_d,
              pc_f,
              instr_f,
              aluout_m, writedata_m, result_w, instr_d,
              readdata_m);
              
endmodule

module controller(input  logic       clk, reset,
                  input  logic [5:0] op, funct,
                  input  logic       flush_e, equal_d,
                  output logic       memwrite_m, pcsrc_d, branch_d,
                  output logic       alusrca_e, alusrcb_e,
                  output logic       aluext_e, regdst_e,
                  output logic       memtoreg_e, memtoreg_m, memtoreg_w,
                  output logic       regwrite_e, regwrite_m, regwrite_w,
                  output logic       jump_d, jr_d,
                  output logic       jal_w,
                  output logic [3:0] alucontrol_e);
                  
  logic        jump_d;
  logic        jr_d;
  logic        jal_d,        jal_e,        jal_m,      jal_w;
  logic        pcsrc_d;
  logic        branch_d;
  logic        bne_d;
  logic        equal_d;
  logic        regwrite_d,   regwrite_e,   regwrite_m, regwrite_w;
  logic        memtoreg_d,   memtoreg_e,   memtoreg_m, memtoreg_w;
  logic        memwrite_d,   memwrite_e,   memwrite_m;
  logic [3:0]  alucontrol_d, alucontrol_e;
  logic        alusrca_d,     alusrca_e;
  logic        alusrcb_d,     alusrcb_e;
  logic        aluext_d,     aluext_e;
  logic        regdst_d,     regdst_e;
  
  logic [2:0]  aluop;  // Caution all bandwidths when modifying one presence.


  maindec md(op, funct, memtoreg_d, memwrite_d, branch_d, bne_d,
             alusrca_d, alusrcb_d, aluext_d, regdst_d, regwrite_d, jump_d, jr_d, jal_d, aluop);
  aludec  ad(funct, aluop, alucontrol_d);
  assign pcsrc_d = branch_d & (equal_d ^ bne_d);
  
  floprc #(1) decode2execute_1(clk, reset, flush_e,   regwrite_d,   regwrite_e);
  floprc #(1) decode2execute_2(clk, reset, flush_e,   memtoreg_d,   memtoreg_e);
  floprc #(1) decode2execute_3(clk, reset, flush_e,   memwrite_d,   memwrite_e);
  floprc #(4) decode2execute_4(clk, reset, flush_e,   alucontrol_d, alucontrol_e);
  floprc #(1) decode2execute_9(clk, reset, flush_e,   alusrca_d,     alusrca_e);
  floprc #(1) decode2execute_5(clk, reset, flush_e,   alusrcb_d,     alusrcb_e);
  floprc #(1) decode2execute_6(clk, reset, flush_e,   aluext_d,     aluext_e);
  floprc #(1) decode2execute_7(clk, reset, flush_e,   regdst_d,     regdst_e);
  floprc #(1) decode2execute_8(clk, reset, flush_e,   jal_d,        jal_e);
  
  flopr #(1) execute2memory_1(clk, reset,   regwrite_e,   regwrite_m); 
  flopr #(1) execute2memory_2(clk, reset,   memtoreg_e,   memtoreg_m);
  flopr #(1) execute2memory_3(clk, reset,   memwrite_e,   memwrite_m);
  flopr #(1) execute2memory_4(clk, reset,   jal_e,        jal_m);
  
  flopr #(1) memory2writeback_1(clk, reset, regwrite_m,   regwrite_w);
  flopr #(1) memory2writeback_2(clk, reset, memtoreg_m,   memtoreg_w);
  flopr #(1) memory2writeback_3(clk, reset, jal_m,        jal_w);
endmodule

module maindec(input  logic [5:0] op, funct,
               output logic       memtoreg, memwrite,
               output logic       branch, bne,
               output logic       alusrca, alusrcb, aluext,
               output logic       regdst, regwrite,
               output logic       jump, jr, jal,
               output logic [2:0] aluop);
  
  assign bne = (op == 6'b000101);
  assign jr = (op == 6'b000000) & (funct[5:1] == 5'b00100);  // JR or JALR
  assign jal = (op == 6'b000011) | ((op == 6'b000000) & (funct == 6'b001001));  // JAL or JALR
  
  logic [11:0] controls;
  assign {
                                 regwrite,
                                   regdst,
                                     alusrca,
                                       alusrcb,
                                         aluext,
                                           branch,
                                             memwrite,
                                               memtoreg,
                                                 jump,
                                                   aluop
  } = controls;
  always_comb
    case(op)
      6'b000000: case(funct) // RTYPE
          6'b001000: controls <= 12'b0_0_0_0_0_0_0_0_1_000; // JR
          6'b001001: controls <= 12'b1_0_0_0_0_0_0_0_1_000; // JAL
          6'b000000: controls <= 12'b1_1_1_0_0_0_0_0_0_110; // SLL
          6'b000010: controls <= 12'b1_1_1_0_0_0_0_0_0_110; // SRL
          6'b000011: controls <= 12'b1_1_1_0_0_0_0_0_0_110; // SRA
          default:   controls <= 12'b1_1_0_0_0_0_0_0_0_110; // other RTYPE instrs
        endcase
      6'b100011: controls <= 12'b1_0_0_1_0_0_0_1_0_000; // LW
      6'b101011: controls <= 12'b0_0_0_1_0_0_1_0_0_000; // SW
      6'b000100: controls <= 12'b0_0_0_0_0_1_0_0_0_001; // BEQ
      6'b000101: controls <= 12'b0_0_0_0_0_1_0_0_0_001; // BNE
      6'b001000: controls <= 12'b1_0_0_1_0_0_0_0_0_000; // ADDI
      6'b001100: controls <= 12'b1_0_0_1_1_0_0_0_0_010; // ANDI
      6'b001101: controls <= 12'b1_0_0_1_1_0_0_0_0_011; // ORI
      6'b001010: controls <= 12'b1_0_0_1_0_0_0_0_0_100; // SLTI
      6'b000010: controls <= 12'b0_0_0_0_0_0_0_0_1_000; // J
      6'b000011: controls <= 12'b1_0_0_0_0_0_0_0_1_000; // JAL
      default:   controls <= 12'bx_x_x_x_x_x_x_x_x_xxx; // illegal op
    endcase
endmodule

module aludec(input  logic [5:0] funct,
              input  logic [2:0] aluop,
              output logic [3:0] alucontrol);
  always_comb
    case(aluop)
      3'b000: alucontrol <= 4'b0010;  // add (for lw/sw/addi)
      3'b001: alucontrol <= 4'b0110;  // sub (for beq)
      3'b010: alucontrol <= 4'b0000;  // and (for andi)
      3'b011: alucontrol <= 4'b0001;  // or  (for ori)
      3'b100: alucontrol <= 4'b0111;  // slt (for slti)
      default: case(funct)          // R-type instructions
          6'b100000: alucontrol <= 4'b0010; // add
          6'b100010: alucontrol <= 4'b0110; // sub
          6'b100100: alucontrol <= 4'b0000; // and
          6'b100101: alucontrol <= 4'b0001; // or
          6'b101010: alucontrol <= 4'b0111; // slt
          6'b000000: alucontrol <= 4'b1000; // sll
          6'b000010: alucontrol <= 4'b1001; // srl
          6'b000011: alucontrol <= 4'b1010; // sra
          6'b000100: alucontrol <= 4'b1000; // sllv
          6'b000110: alucontrol <= 4'b1001; // srlv
          6'b000111: alucontrol <= 4'b1010; // srav
          default:   alucontrol <= 4'bxxxx; // ???
        endcase
    endcase
endmodule

module datapath(input  logic        clk, reset,
                input  logic        pcsrc_d, branch_d,
                input  logic        alusrca_e, alusrcb_e,
                input  logic        aluext_e, regdst_e,
                input  logic        memtoreg_e, memtoreg_m, memtoreg_w,
                input  logic        regwrite_e, regwrite_m, regwrite_w,
                input  logic        jump_d, jr_d,
                input  logic        jal_w,
                input  logic [3:0]  alucontrol_e,
                output logic        flush_e, equal_d,
                output logic [31:0] pc_f,
                input  logic [31:0] instr_f,
                output logic [31:0] aluout_m, writedata_m, result_w, instr_d,
                input  logic [31:0] readdata_m);

  logic [31:0] pcnextbr,  pcbranch_d;
  logic [31:0] pcnext;
  logic [31:0] pc_f;
  logic        stall_f;
  logic [31:0] instr_f,   instr_d;
  logic [31:0] pcplus4_f, pcplus4_d,   pcplus4_e,   pcplus4_m,   pcplus4_w;
  logic [31:0]            jmpdst_d;
  logic                   flush_d;
  logic                   stall_d;
  logic                   forwarda_d;
  logic                   forwardb_d;
  logic [31:0]            brarg1_d;
  logic [31:0]            brarg2_d;
  logic [31:0]            rfout1_d;
  logic [31:0]            rfout2_d;
  logic [31:0]            rfread1_d,   rfread1_e;
  logic [31:0]            rfread2_d,   rfread2_e;
  logic                                flush_e;
  logic [31:0]                         srcans_e;  // srcA for non shift instrs
  logic [31:0]                         srca_e;
  logic [31:0]                         srcb_e;
  logic [4:0]             rs_d,        rs_e;
  logic [4:0]             rt_d,        rt_e;
  logic [4:0]             rd_d,        rd_e;
  logic [4:0]             shamt_d,     shamt_e;
  logic [31:0]            zeroimm_d,   zeroimm_e;
  logic [31:0]            signimm_d,   signimm_e;
  logic [31:0]            signimmsh_d;
  logic [31:0]                         extimm_e;
  logic [1:0]                          forwarda_e;
  logic [1:0]                          forwardb_e;
  logic [4:0]                          writereg_e,  writereg_m,  writereg_w;
  logic [4:0]                                                    rfaddrin3_w;
  logic [31:0]                         aluout_e,    aluout_m,    aluout_w;
  logic [31:0]                         writedata_e, writedata_m;
  logic [31:0]                                                   result_w;
  logic [31:0]                                                   rfin3_w;
  logic [31:0]                                      readdata_m,  readdata_w;
    
  // next PC logic
  flopenr #(32) pcreg(clk, reset, ~stall_f, pcnext, pc_f);
  adder       pcadd1_f(pc_f, 32'b100, pcplus4_f);
  sl2         immsh_d(signimm_d, signimmsh_d);
  adder       pcadd2_d(pcplus4_d, signimmsh_d, pcbranch_d);
  mux2 #(32)  pcbrmux(pcplus4_f, pcbranch_d, pcsrc_d, pcnextbr);
  mux2 #(32)  jmpdstmux({pcplus4_d[31:28], instr_d[25:0], 2'b00},
                        rfread1_d, jr_d, jmpdst_d);
  mux2 #(32)  pcjmpmux(pcnextbr, jmpdst_d, jump_d, pcnext);
  assign flush_d = pcsrc_d | jump_d;

  // register file logic
  assign rs_d = instr_d[25:21];
  assign rt_d = instr_d[20:16];
  assign rd_d = instr_d[15:11];
  assign shamt_d = instr_d[10:6];
  regfile     rf(clk, reset, regwrite_w, rs_d, rt_d, 
                 rfaddrin3_w, rfin3_w, rfout1_d, rfout2_d);
  mux2 #(32)  brmux1_d(rfout1_d, aluout_m, forwarda_d, rfread1_d);
  mux2 #(32)  brmux2_d(rfout2_d, aluout_m, forwardb_d, rfread2_d);
  eqcmp       brpred_d(rfread1_d, rfread2_d, equal_d);
  mux2 #(5)   wrmux_e(rt_e, rd_e, regdst_e, writereg_e);
  mux2 #(5)   wrmux_w(writereg_w, 5'd31, jal_w, rfaddrin3_w);
  mux2 #(32)  resmux2_w(result_w, pcplus4_w, jal_w, rfin3_w);
  mux2 #(32)  resmux_w(aluout_w, readdata_w, memtoreg_w, result_w);
  signext     se_d(instr_d[15:0], signimm_d);
  zeroext     ze_d(instr_d[15:0], zeroimm_d);

  // ALU logic
  mux3 #(32)  fwamux_e(rfread1_e, result_w, aluout_m, forwarda_e, srcans_e);
  mux3 #(32)  fwbmux_e(rfread2_e, result_w, aluout_m, forwardb_e, writedata_e);
  mux2 #(32)  extmux_e(signimm_e, zeroimm_e, aluext_e, extimm_e);
  mux2 #(32)  srcamux_e(srcans_e, {27'b0, shamt_e}, alusrca_e, srca_e);
  mux2 #(32)  srcbmux_e(writedata_e, extimm_e, alusrcb_e, srcb_e);
  alu         alu(srca_e, srcb_e, alucontrol_e, aluout_e, );
  
  // hazard unit
  hazardunit  hz(memtoreg_e, memtoreg_m, regwrite_e, regwrite_m, regwrite_w, branch_d, jr_d,
                 rs_d, rt_d, rs_e, rt_e, writereg_e, writereg_m, writereg_w,
                 forwarda_e, forwardb_e,
                 stall_d, stall_f, flush_e, forwarda_d, forwardb_d);
  
  // pipeline registers
  flopenrc #(32) fetch2decode_11(clk, reset, ~stall_d, flush_d, instr_f,     instr_d);
  flopenrc #(32) fetch2decode_12(clk, reset, ~stall_d, flush_d, pcplus4_f,   pcplus4_d);
  
  floprc #(32) decode2execute_11(clk, reset, flush_e,   rfread1_d,   rfread1_e);
  floprc #(32) decode2execute_12(clk, reset, flush_e,   rfread2_d,   rfread2_e);
  floprc #(5)  decode2execute_13(clk, reset, flush_e,   rs_d,        rs_e);
  floprc #(5)  decode2execute_14(clk, reset, flush_e,   rt_d,        rt_e);
  floprc #(5)  decode2execute_15(clk, reset, flush_e,   rd_d,        rd_e);
  floprc #(5)  decode2execute_19(clk, reset, flush_e,   shamt_d,     shamt_e);
  floprc #(32) decode2execute_16(clk, reset, flush_e,   signimm_d,   signimm_e);
  floprc #(32) decode2execute_17(clk, reset, flush_e,   zeroimm_d,   zeroimm_e);
  floprc #(32) decode2execute_18(clk, reset, flush_e,   pcplus4_d,   pcplus4_e);
  
  flopr #(32) execute2memory_11(clk, reset,   aluout_e,    aluout_m);
  flopr #(32) execute2memory_12(clk, reset,   writedata_e, writedata_m);
  flopr #(5)  execute2memory_13(clk, reset,   writereg_e,  writereg_m);
  flopr #(32) execute2memory_14(clk, reset,   pcplus4_e,   pcplus4_m);
  
  flopr #(32) memory2writeback_11(clk, reset, readdata_m,  readdata_w);
  flopr #(32) memory2writeback_12(clk, reset, aluout_m,    aluout_w);
  flopr #(5)  memory2writeback_13(clk, reset, writereg_m,  writereg_w);
  flopr #(32) memory2writeback_14(clk, reset, pcplus4_m,   pcplus4_w);
endmodule

module regfile(input  logic        clk, reset,
               input  logic        we3, 
               input  logic [4:0]  ra1, ra2, wa3, 
               input  logic [31:0] wd3, 
               output logic [31:0] rd1, rd2);

  logic [31:0] rf[31:0];
  integer i;

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

  assign rd1 = (ra1 != 0) ? rf[ra1] : 0;
  assign rd2 = (ra2 != 0) ? rf[ra2] : 0;
endmodule

module hazardunit(input  logic        memtoreg_e, memtoreg_m, regwrite_e, regwrite_m, regwrite_w,
                  input  logic        branch_d, jr_d,
                  input  logic [4:0]  rs_d, rt_d, rs_e, rt_e, writereg_e, writereg_m, writereg_w,
                  output logic [1:0]  forwarda_e, forwardb_e,
                  output logic        stall_f, stall_d, flush_e, forwarda_d, forwardb_d);
  // forwarding of data hazard
  always_comb
    if (rs_e != '0 && rs_e == writereg_m && regwrite_m) forwarda_e = 2'b10;
    else if (rs_e != '0 && rs_e == writereg_w && regwrite_w) forwarda_e = 2'b01;
    else forwarda_e = 2'b00;
  always_comb
    if (rt_e != '0 && rt_e == writereg_m && regwrite_m) forwardb_e = 2'b10;
    else if (rt_e != '0 && rt_e == writereg_w && regwrite_w) forwardb_e = 2'b01;
    else forwardb_e = 2'b00;
  
  // forwarding of control hazard
  assign forwarda_d = (rs_d != 0) & (rs_d == writereg_m) & regwrite_m;
  assign forwardb_d = (rt_d != 0) & (rt_d == writereg_m) & regwrite_m;
  
  // stalls of data hazard (LW) and control hazard
  logic lwstall, branchstall;
  assign lwstall = ((rs_d == rt_e) | (rt_d == rt_e)) & memtoreg_e;
  assign branchstall = ((branch_d | jr_d) & regwrite_e & (writereg_e == rs_d | writereg_e == rt_d))
                     | ((branch_d | jr_d) & memtoreg_m & (writereg_m == rs_d | writereg_m == rt_d));
  assign stall_f = lwstall | branchstall;
  assign stall_d = stall_f;
  assign flush_e = stall_d;
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
    if       (reset) q <= 0;
    else if (en)     q <= d;
endmodule

module floprc #(parameter WIDTH = 8)
              (input  logic             clk, reset, clear,
               input  logic [WIDTH-1:0] d, 
               output logic [WIDTH-1:0] q);

  always_ff @(posedge clk, posedge reset)
    if       (reset) q <= 0;
    else if (clear) q <= 0;
    else            q <= d;
endmodule

module flopenrc #(parameter WIDTH = 8)
              (input  logic             clk, reset, en, clear,
               input  logic [WIDTH-1:0] d, 
               output logic [WIDTH-1:0] q);

  always_ff @(posedge clk, posedge reset)
    if       (reset) q <= 0;
    else if (en & clear) q <= 0;
    else if (en)    q <= d;
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

module eqcmp(input  logic [31:0] a, b,
              output logic        eq);
  assign eq = (a == b);
endmodule

module alu(input  logic [31:0] a, b,
           input  logic [3:0]  alucontrol,
           output logic [31:0] result,
           output logic        zero);

  logic [31:0] condinvb, sum;

  assign condinvb = alucontrol[2] ? ~b : b;
  assign sum = a + condinvb + alucontrol[2];

  always_comb
    if (alucontrol[3] == 1'b0)
      case (alucontrol[1:0])
        2'b00: result = a & b;
        2'b01: result = a | b;
        2'b10: result = sum;
        2'b11: result = sum[31];
      endcase
    else case (alucontrol[1:0])
            2'b00: result = b << a[4:0];  // 1000: sll
            2'b01: result = b >> a[4:0];  // 1001: srl
            2'b10: result = b >>> a[4:0];  // 1010: sra
            default: result = 'x;
          endcase

  assign zero = (result == 32'b0);
endmodule
