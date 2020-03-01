// Seven Segment Display
module disp(input  logic [2:0]  clk,
            input  logic        reset,
            input  logic [31:0] in,
            output logic [7:0]  an,
            output logic        dp,
            output logic [6:0]  a2g);
    // Turn off floating point display
    assign dp = 1;
    
    // Display contents for a single digital tube
    logic [3:0] digit;
    logic [6:0] out;
    always_comb
    if (reset == 0)
        case (digit)
            'h0: out = 7'b0111111;
            'h1: out = 7'b0000110;
            'h2: out = 7'b1011011;
            'h3: out = 7'b1001111;
            'h4: out = 7'b1100110;
            'h5: out = 7'b1101101;
            'h6: out = 7'b1111101;
            'h7: out = 7'b0000111;
            'h8: out = 7'b1111111;
            'h9: out = 7'b1100111;
            'hA: out = 7'b1110111;
            'hB: out = 7'b1111100;
            'hC: out = 7'b0111001;
            'hD: out = 7'b1011110;
            'hE: out = 7'b1111001;
            'hF: out = 7'b1110001;
        endcase
    else out = 7'b0;
    assign a2g = ~out;
    
    // Time division
    always_comb
    begin
        an = 8'b1111_1111; digit = 4'b0;
        if (reset == 0)
            case (clk)
                3'd0: begin an[0] = 0; digit = in[3:0]; end
                3'd1: begin an[1] = 0; digit = in[7:4]; end
                3'd2: begin an[2] = 0; digit = in[11:8]; end
                3'd3: begin an[3] = 0; digit = in[15:12]; end
                3'd4: begin an[4] = 0; digit = in[19:16]; end
                3'd5: begin an[5] = 0; digit = in[23:20]; end
                3'd6: begin an[6] = 0; digit = in[27:24]; end
                3'd7: begin an[7] = 0; digit = in[31:28]; end
            endcase
    end
        
endmodule
