module clkd(input  logic        clk,
            input  logic        reset,
            output logic [26:0] clkdiv);
    always_ff @ (posedge clk)
        if (reset) clkdiv <= 27'b0;
        else clkdiv <= clkdiv + 1;
endmodule
