/**
 * en         : en in cache module
 * w_en      : cache writing enable signal, from w_en in cache module
 * hit, dirty : from set module
 *
 * cw_en      : writing enable signal to cache line
 * mw_en      : writing enable signal to memory , controls whether to write to memory
 * set_valid  : control signal for cache line
 * set_dirty  : control signal for cache line
 * offset_sel : control signal for cache line and this may be used in other places
 */

`define NORMAL 2'b10
`define FETCH 2'b01
`define WRITE 2'b00

module cache_controller #(
	parameter TAG_WIDTH    = `CACHE_T,
		       SET_WIDTH    = `CACHE_S,
		       OFFSET_WIDTH = `CACHE_B,
	           COUNT_MAX = 2 ** (OFFSET_WIDTH - 2)
)(
	input  logic clk, reset, en,
	input  logic w_en, set_hit, dirty,
	input  logic [TAG_WIDTH - 1:0] tag,
    input  logic [SET_WIDTH - 1:0] idx,
    input  logic [OFFSET_WIDTH - 1:0] ofs,
	input  logic [TAG_WIDTH - 1:0] line_tag,
	input  logic [31:0] w_data, cr_data, mr_data,
	output logic update_en, cw_en, mw_en,
	output logic [31:0] mw_addr, mw_data, cw_data,
	output logic [OFFSET_WIDTH - 1:0] block_idx,
	output logic strategy_en, hit, replace
);

    // finite state machine (FSM)
    logic        counter_reset;
    logic [31:0] count;
    logic [1:0]  state;
    always_ff @ (posedge clk)
    begin
        if (reset) state <= `NORMAL;
        else if (en) begin
            case (state)
                `FETCH: if (count == COUNT_MAX - 1) state <= `NORMAL;
                `WRITE: if (count == COUNT_MAX - 1) state <= `FETCH;
                default: if (!set_hit) state <= (dirty ? `WRITE : `FETCH);
            endcase
        end
    end

    // counter for fetch / write
    always_ff @ (posedge clk)
    begin
        if (reset | counter_reset) count <= '0;
        else count <= count + 1'b1;
    end
    
    // control signals
    assign strategy_en = 1'b1;
	assign mw_data = cr_data;
    
    logic normal;
    assign normal = state[1];
    assign replace = ~normal;
    assign hit = normal & set_hit;
    
    logic [OFFSET_WIDTH - 1:0] pos;
    assign pos = count[OFFSET_WIDTH - 1:0] << 2;
    assign block_idx = (normal != 0 ? ofs : pos);

    always_comb
    begin
        {update_en, cw_en, mw_en, mw_addr, cw_data, counter_reset} = '0;
        if (en) begin
            case (state)
                `FETCH: begin
                    update_en = count == COUNT_MAX - 1 ? 1'b1 : 1'b0; cw_en = 1'b1;
                    mw_addr = {tag, idx, pos};
                    cw_data = mr_data;
                end
                `WRITE: begin
                    update_en = 1'b0; cw_en = 1'b0; mw_en = 1'b1;
                    mw_addr = {line_tag, idx, pos};
                    if (count == COUNT_MAX - 1) counter_reset = 1'b1;
                end
                default: begin
                    update_en = 1'b0; cw_en = w_en;
                    cw_data = w_data;
                    counter_reset = !set_hit;
                end
            endcase
        end  // if
    end  // always_comb

endmodule
