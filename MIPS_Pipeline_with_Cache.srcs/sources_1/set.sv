`include "cache.svh"
/**
 * ctls       : control signals from cache_controller
 * addr       : cache read/write address from processor
 * write_data : cache write data from processor
 * mread_data : data read from memory
 * 
 * hit        : whether cache set hits
 * dirty      : from the cache line selected by addr (cache line's tag is equal to addr's tag)
 */

module set #(
	parameter TAG_WIDTH    = `CACHE_T,
		      OFFSET_WIDTH = `CACHE_B,
		      LINES        = `CACHE_E
)(
	input  logic        clk, reset, en,
	input  logic        w_en, update_en,
	input  logic        strategy_en, replace,
	input  logic [31:0] addr, write_data,
	input  logic [OFFSET_WIDTH-1:0] block_idx,
	output logic        hit, dirty,
	output logic [TAG_WIDTH-1:0] tag,
	output logic [31:0] read_data
);

    logic [$clog2(LINES) - 1:0]  line_index, hline_index, alloc_line;
    logic [TAG_WIDTH - 1:0]      lines_tag[LINES], set_tag;
    logic                        lines_valid[LINES], lines_dirty[LINES], lines_en[LINES];
    logic [31:0]                 lines_rdata[LINES];
    
    assign set_tag = addr[31:32 - TAG_WIDTH];
    
    // replace
	// currently we use RANDOM replace strategy, for the sake of simplicity. So no need of signal strategy_en.
	always @ (posedge clk, posedge reset)
	   if (reset | ~replace) alloc_line = $random % LINES;
	assign line_index = replace ? alloc_line : hline_index;
    
    // line_index, muxes and demuxes
    always_comb
    begin
        hline_index = 'x;
        for (integer i = 0; i < LINES; i = i+1)
            if ((set_tag == lines_tag[i]) & lines_valid[i]) hline_index = i;
    end
    // priority encoding: if multiple tags match and valid, then the line with highest index has priority
    
    always_comb
        for (integer i = 0; i < LINES; i = i+1) lines_en[i] = (line_index == i) & en;
    
    assign read_data = lines_rdata[line_index];
    assign hit = (hline_index !== 'x) | replace;
    assign tag = lines_tag[alloc_line];
    assign dirty = lines_dirty[alloc_line];  // ???
    
    // cache lines
    line lines[LINES] (
        .clk        ( clk ),
        .reset      ( reset ),
        .en         ( lines_en ),
        .offset     ( block_idx ),
        .w_en       ( w_en ),
        .update_en  ( update_en ),
        .set_tag    ( set_tag ),
        .write_data ( write_data ),
        .valid      ( lines_valid ),
        .dirty      ( lines_dirty ),
        .tag        ( lines_tag ),
        .read_data  ( lines_rdata )
	);
	
endmodule
