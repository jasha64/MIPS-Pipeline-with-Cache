`include "cache.svh"
/**
 * w_en: write enable
 */
module line #(
	parameter TAG_WIDTH    = `CACHE_T,
		      OFFSET_WIDTH = `CACHE_B,
		      BYTES = 2**(OFFSET_WIDTH-2)
)(
	input  logic                       clk, reset, en,
	input  logic [OFFSET_WIDTH - 1:0]  offset,
	input  logic                       w_en, update_en,
	input  logic [TAG_WIDTH - 1:0]     set_tag,
	input  logic [31:0]                write_data,
	output logic                       valid,
	output logic                       dirty,
	output logic [TAG_WIDTH - 1:0]     tag,
	output logic [31:0]                read_data
);

    logic [31:0] data[BYTES];
    logic [OFFSET_WIDTH - 3:0] aligned_ofs;
    assign aligned_ofs = offset[OFFSET_WIDTH - 1:2];
    
    assign read_data = data[aligned_ofs];
    always_ff @ (posedge clk, posedge reset)
        if (reset)
        begin
            valid <= 1'b0;
            dirty <= 1'b0;
            tag <= '0;
            for (int i = 0; i < BYTES; i++) data[i] <= '0;
        end
        else if (en) begin
            if (update_en)  // validate
            begin
                valid <= 1'b1;
                  // if a line becomes valid, it never invalidates again unless the program runs to an end
                dirty <= 1'b0;
                tag <= set_tag;
            end  // if
            if (w_en)  // write
            begin
                if (~update_en) dirty <= 1'b1;
                data[aligned_ofs] <= write_data;
            end  // if
        end  // elseif

endmodule
