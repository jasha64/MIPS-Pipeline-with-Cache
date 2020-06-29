// �洢��ӳ��IO�����豸����ĵ�ַ�ֱ�Ϊ��
// 0x80: �豸״̬�˿�
// 0x84: LED
// 0x88: Switch��8λ
// 0x8C: Switch��8λ

module dmem_with_io(
    input  logic        clk, we,
    input  logic [31:0] a, wd,
    output logic [31:0] rd,
    
    input  logic        rst,
    input  logic        BTNL, BTNR,
    input  logic [15:0] SW,
    output logic [11:0] LED
);

  logic        pread, pwrite, memwrite;
  logic [31:0] readdata1, readdata2;
  
  assign pread = a[7];
  assign pwrite = a[7] & we;
  assign memwrite = (!a[7]) & we;
  mux2 #(32) rdmux(readdata1, readdata2, a[7], rd);  // 0x8*��ַ��Ӧ����I/O hole
  dmem dmem(clk, memwrite, a, wd, readdata1);
  IO io(clk, rst, pread, pwrite, a[3:2], wd, readdata2, BTNL, BTNR, SW, LED);
  
endmodule

module IO(
    input  logic        clk,
    input  logic        reset,
    input  logic        pRead,
    input  logic        pWrite,
    input  logic [1:0]  addr,
    input  logic [31:0] pWriteData,
    output logic [31:0] pReadData,
    
    input  logic        buttonL, // LED���
    input  logic        buttonR, // Switch����
    input  logic [15:0] switch,
    output logic [11:0] led
);
    logic [1:0]  status;
    logic [15:0] switch1;
    logic [11:0] led1;
    
    always_ff @(negedge clk) begin
        if (reset) begin
            status  <= 2'b00;
            led1    <= 12'h00;
            switch1 <= 16'h00;
        end
        else begin
            // ����λ���Ѿ����ã���������������
            if (buttonR) begin
                status[1] <= 1;
                switch1   <= switch;
            end
            
            // LEDs�Ѿ�׼���ã��������������
            if (buttonL) begin
                status[0] <= 1;
                led       <= led1;
            end

            // ����������˿����(LED)
            if (pWrite & (addr==2'b01)) begin
                led1 <= pWriteData[11:0];
                status[0] <= 0;
            end
            
            // ������
            if (pRead) begin
                // 11:��������˿�(��), 10:��������˿�(��)
                // 01:��������˿�(LED), 00: ״̬�˿�
                case (addr)
                    2'b11: pReadData <= {24'b0, switch1[15:8]};
                    2'b10: pReadData <= {24'b0, switch1[7:0]};
                    2'b00: pReadData <= {24'b0, 6'b000000, status};
                    default: pReadData <= 32'b0;
                endcase
            end
        end // if
    end // always_ ff

endmodule
