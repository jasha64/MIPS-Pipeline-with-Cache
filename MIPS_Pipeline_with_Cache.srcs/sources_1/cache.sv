`include "cache.svh"

/**
 * NOTE: The sum of TAG_WIDTH, SET_WIDTH and OFFSET_WIDTH should be 32
 *
 * TAG_WIDTH    : (t) tag bits
 * SET_WIDTH    : (s) set index bits, the number of sets is 2**SET_WIDTH
 * OFFSET_WIDTH : (b) block offset bits
 * LINES        : number of lines per set
 *
 * stall        : inorder to synchronize instruction memroy cache and data memroy cache, you may need this so that two caches will write data at most once per instruction respectively.
 *
 * input_ready  : whether input data from processor are ready
 * addr         : cache read/write address from processor
 * write_data   : cache write data from processor
 * w_en         : cache write enable
 * hit          : whether cache hits
 * read_data    : data read from cache
 *
 * maddr        : memory address 
 * mwrite_data  : data written to memory
 * m_wen        : memory write enable
 * mread_data   : data read from memory
 */
module cache #(
	parameter TAG_WIDTH    = `CACHE_T,
		       SET_WIDTH    = `CACHE_S,
		       OFFSET_WIDTH = `CACHE_B,
		       LINES        = `CACHE_E,
		       SETS         = 2**SET_WIDTH
)(
    // do nothing when stall == 1
	input  logic        clk, reset, stall,

	// interface with CPU
	// issues a read/write request when input_ready == 1
	input  logic        input_ready,
	input  logic [31:0] addr, write_data,
	input  logic        w_en,
	output logic        hit,
	output logic [31:0] read_data,

	// interface with memory
	output logic [31:0] maddr, mwrite_data,
	output logic        m_wen,
	input  logic [31:0] mread_data
);

    logic [SET_WIDTH-1:0] set_index;
    logic                 en, update_en, cw_en;
    logic                 dirty, sets_dirty[SETS];
    logic                 replace;
    logic                 set_hit, sets_hit[SETS];
    logic                 sets_en[SETS];
    logic [31:0]          sets_rdata[SETS];
    logic [TAG_WIDTH-1:0] line_tag, lines_tag[SETS];
    logic [31:0]          cwrite_data;
    logic [TAG_WIDTH-1:0] tag;
    logic [SET_WIDTH-1:0] idx;
    logic [OFFSET_WIDTH-1:0] ofs, block_idx;
    
    assign { tag, idx, ofs } = addr;
    assign en = ~stall & input_ready;
    
    // cache sets
    set sets[SETS] (
        .clk         ( clk ),
        .reset       ( reset ),
        .en          ( sets_en ),
        .w_en        ( cw_en ),
        .update_en   ( update_en ),
        .strategy_en ( 1'b1 ),  // unused
        .replace     ( replace ),
        .addr        ( addr ),
        .write_data  ( cwrite_data ),
        .block_idx   ( block_idx ),
        .hit         ( sets_hit ),
        .dirty       ( sets_dirty ),
        .tag         ( lines_tag ),
        .read_data   ( sets_rdata )
	);
	
	// set_index, muxes and demuxes
    assign set_index = idx;
	assign read_data = sets_rdata[set_index];
    assign set_hit = sets_hit[set_index];
    assign dirty = sets_dirty[set_index];
    assign line_tag = lines_tag[set_index]; 
    
    always_comb
        for (integer i = 0; i < SETS; i++) sets_en[i] = (set_index == i) & en;

    // FSM and control signals
    cache_controller ctrl(
        .clk          ( clk ),
        .reset        ( reset ),
        .en           ( en ),
        .w_en         ( w_en ),
        .set_hit      ( set_hit ),
        .dirty        ( dirty ),
        .tag          ( tag ),
        .idx          ( idx ),
        .ofs          ( ofs ),
        .line_tag     ( line_tag ),
        .w_data       ( write_data ),
        .cr_data      ( read_data ),
        .mr_data      ( mread_data ),
        .update_en    ( update_en ),
        .cw_en        ( cw_en ),
        .mw_en        ( m_wen ),
        .mw_addr      ( maddr ),
        .mw_data      ( mwrite_data ),
        .cw_data      ( cwrite_data ),
        .block_idx    ( block_idx ),
        .strategy_en  (  ),
        .hit          ( hit ),
        .replace      ( replace )
    );

endmodule
