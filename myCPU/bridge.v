`define RREQ_RST        5'b00001
`define RREQ_INST_VALID 5'b00010
`define RREQ_DATA_VALID 5'b00100
`define RREQ_BLOCK      5'b01000
`define RREQ_RET        5'b10000

`define WREQ_RST        5'b00001
`define WREQ_START      5'b00010
`define WREQ_AW_VALID   5'b00100
`define WREQ_W_VALID    5'b01000
`define WREQ_RET        5'b10000

`define R_RST           3'b001
`define R_VALID         3'b010
`define R_RET           3'b100

`define W_RST           3'b001
`define W_VALID         3'b010
`define W_RET           3'b100

module bridge(
    input wire        clk,
    input wire        reset,
    input wire ex,
    // inst sram interface
    input wire        inst_sram_req,
    input wire        inst_sram_wr,
    input wire [ 1:0] inst_sram_size,
    input wire [31:0] inst_sram_addr,
    input wire [ 3:0] inst_sram_wstrb,
    input wire [31:0] inst_sram_wdata,
    output wire        inst_sram_addr_ok,
    output wire        inst_sram_data_ok,
    output wire [31:0] inst_sram_rdata,

    // data sram interface
    input wire        data_sram_req,
    input wire        data_sram_wr,
    input wire [ 1:0] data_sram_size,
    input wire [31:0] data_sram_addr,
    input wire [ 3:0] data_sram_wstrb,
    input wire [31:0] data_sram_wdata,
    output wire        data_sram_addr_ok,
    output wire        data_sram_data_ok,
    output wire [31:0] data_sram_rdata,

    //read request
    output wire [ 3:0] arid,
    output wire [31:0] araddr,
    output wire [ 7:0] arlen,
    output wire [ 2:0] arsize,
    output wire [ 1:0] arburst,
    output wire [ 1:0] arlock,
    output wire [ 3:0] arcache,
    output wire [ 2:0] arprot,
    output wire        arvalid,

    input wire        arready,

    //read response
    output wire rready,

    input wire [ 3:0] rid,
    input wire [31:0] rdata,
    input wire [ 1:0] rresp,
    input wire        rlast,
    input wire        rvalid,

    //write request
    output wire [ 3:0] awid,
    output wire [31:0] awaddr,
    output wire [ 7:0] awlen,
    output wire [ 2:0] awsize,
    output wire [ 1:0] awburst,
    output wire [ 1:0] awlock,
    output wire [ 3:0] awcache,
    output wire [ 2:0] awprot,
    output wire        awvalid,

    input wire        awready,

    //write data
    output wire [ 3:0] wid,
    output wire [31:0] wdata,
    output wire [ 3:0] wstrb,
    output wire        wlast,
    output wire        wvalid,

    input wire        wready,

    //write response
    output wire        bready,

    input wire [ 3:0] bid,
    input wire [ 1:0] bresp,
    input wire        bvalid
);

reg [ 4:0] rreq_current_state;
reg [ 4:0] rreq_next_state;
reg [ 4:0] wr_current_state;//wr->write_request
reg [ 4:0] wr_next_state;
reg [ 2:0] r_current_state;
reg [ 2:0] r_next_state;
reg [ 2:0] w_current_state;
reg [ 2:0] w_next_state;
reg        chg_when_req;

reg [31:0] inst_addr_delay;

reg        chg_data_when_req;

reg        rready_buf;
reg        bready_buf;
reg        inst_sram_data_ok_buf;
reg [31:0] inst_sram_rdata_buf;
reg        data_sram_data_ok_buf;
reg [31:0] data_sram_rdata_buf;
reg [ 3:0] rid_buf;

reg [ 3:0] arid_buf;
reg [31:0] araddr_buf;
reg [ 2:0] arsize_buf;
reg        arvalid_buf;

reg [31:0] wraddr_buf;
reg [ 2:0] wrsize_buf;
reg        wrvalid_buf;    

reg [31:0] wdata_buf;
reg [ 3:0] wstrb_buf;
reg        wvalid_buf;

assign arid = arid_buf;
assign araddr = araddr_buf;
assign arlen = 8'b0;
assign arsize = arsize_buf;
assign arburst = 2'b01;
assign arlock = 2'b0;
assign arcache = 4'b0;
assign arprot = 3'b0;
assign arvalid = arvalid_buf;

assign wid     = 4'b1;
assign wlast   = 1'b1;

assign awid    = 4'b1;
assign awlen   = 8'b0;
assign awburst = 2'b01;
assign awlock  = 2'b0;
assign awcache = 4'b0;
assign awprot  = 3'b0;

/****************************inst_sram data_sram ********************************/
always@(posedge clk) begin
    if(reset) begin
        inst_addr_delay <= 32'h1c00_0000;
    end
    else if(inst_sram_req) begin
        inst_addr_delay <= inst_sram_addr;
    end
end

always @(posedge clk) begin
    if(reset) begin
        inst_sram_rdata_buf <= 32'b0;
    end
    else if(rvalid && rready && (rid == 4'b0)) begin
        inst_sram_rdata_buf <= rdata;
    end
end

always @(posedge clk) begin
    if(reset) begin
        data_sram_rdata_buf <= 32'b0;
    end
    else if(rvalid && rready && (rid == 4'b1)) begin
        data_sram_rdata_buf <= rdata;
    end
end

assign data_sram_rdata = data_sram_rdata_buf;
assign inst_sram_rdata = inst_sram_rdata_buf;
assign inst_sram_data_ok = (r_current_state == `R_RET && !rid_buf[0] && !chg_data_when_req);
assign data_sram_data_ok = (r_current_state == `R_RET && rid_buf[0]) || (w_current_state == `W_RET && wid == 4'b1);
assign inst_sram_addr_ok = (rreq_current_state == `RREQ_RET) && (arid == 4'b0) && !chg_when_req;
assign data_sram_addr_ok = (rreq_current_state == `RREQ_RET) && (arid == 4'b1) || (wr_current_state == `WREQ_RET) && (awid == 4'b1);

/****************************inst_sram data_sram ********************************/


/****************************AR**************************************************/
always@(posedge clk) begin
    if(reset) begin
        arid_buf <= 4'b0;
    end
    else if(rreq_next_state == `RREQ_DATA_VALID) begin
        arid_buf <= 4'b1;
    end
    else if(rreq_next_state == `RREQ_INST_VALID) begin
        arid_buf <= 4'b0;
    end
end //every time before sending the request, set arid_buf to correct value. then it will automatically stay correct, until next we need to use it.

always@(posedge clk) begin
    if(reset) begin
        araddr_buf <= 32'h1c000000;
    end
    else if(inst_sram_req && ((arid == 4'b0) && (araddr_buf != inst_addr_delay))) begin
        araddr_buf <= inst_sram_addr;
    end
    else if((rreq_next_state == `RREQ_DATA_VALID) && (rreq_current_state != `RREQ_DATA_VALID)) begin
        araddr_buf <= data_sram_addr;
    end
    else if((rreq_next_state == `RREQ_INST_VALID) && (rreq_current_state != `RREQ_INST_VALID)) begin
        araddr_buf <= inst_sram_addr;
    end
end //why not "when rreq_current_state == `RREQ_INST_VALID, araddr_buf <= inst_sram_addr"? because you are not allowed to change any relevant data when waiting for arready.

always@(posedge clk) begin
    if(reset) begin
        arsize_buf <= 3'b0;
    end
    else if(rreq_next_state == `RREQ_DATA_VALID) begin
        arsize_buf <= {1'b0, data_sram_size};
    end
    else if(rreq_next_state == `RREQ_INST_VALID) begin
        arsize_buf <= {1'b0, inst_sram_size};
    end
end

always@(posedge clk) begin
    if(reset) begin
        arvalid_buf <= 1'b0;
    end
    /*else if(rreq_next_state == `RREQ_RST) begin
        arvalid_buf <= 1'b0;
    end*/
    else if(rreq_next_state == `RREQ_RET) begin
        arvalid_buf <= 1'b0;
    end
    else if((rreq_next_state == `RREQ_INST_VALID) || (rreq_next_state == `RREQ_DATA_VALID) || (rreq_current_state == `RREQ_INST_VALID) || (rreq_current_state == `RREQ_DATA_VALID)) begin
        arvalid_buf <= 1'b1;
    end
end
/****************************AR**************************************************/

/****************************R & W *************************************************/
always @(posedge clk) begin
    if(reset) begin
        rid_buf <= 4'b0;
    end
    else if(rvalid) begin
        rid_buf <= rid;
    end
    else begin
        rid_buf <= rid_buf;
    end
end

always @(posedge clk) begin
    if(reset) begin
        rready_buf <= 1'b0;
    end
    /*else if(r_next_state == `R_RST) begin
        rready_buf <= 1'b0;
    end*/
    else if(r_next_state == `R_VALID) begin
        rready_buf <= 1'b1;
    end 
    else 
        rready_buf <= 1'b0;
end
assign rready = rready_buf;

always @(posedge clk) begin
    if(reset) begin
        bready_buf <= 1'b0;
    end
    else if(w_next_state == `W_VALID) begin
        bready_buf <= 1'b1;
    end
    else begin
        bready_buf <= 1'b0;
    end
end
assign bready = bready_buf;

/****************************R & W *************************************************/

//wr_current_state,
always @(posedge clk) begin
    if(reset)
        wr_current_state <= `WREQ_RST;//reset
    else
        wr_current_state <= wr_next_state;
end

//wdata, wdata_buf
always @(posedge clk) begin
    if(reset)
        wdata_buf <= 32'b0;
    else if(wr_next_state == `WREQ_START)
        wdata_buf <= data_sram_wdata;
end
assign wdata = wdata_buf;

//wstrb, wstrb_buf
always @(posedge clk) begin
    if(reset)
        wstrb_buf <= 32'b0;
    else if(wr_next_state == `WREQ_START)
        wstrb_buf <= data_sram_wstrb;
end
assign wstrb = wstrb_buf;   

//wvalid, wvalid_buf
always @(posedge clk) begin
    if(reset)
        wvalid_buf <= 1'b0;
    else if(wr_next_state == `WREQ_W_VALID || wr_next_state == `WREQ_RET && wr_current_state != `WREQ_W_VALID)
        wvalid_buf <= 1'b0;
    else if(wr_next_state == `WREQ_START || wr_next_state == `WREQ_AW_VALID)
        wvalid_buf <= 1'b1;
    else 
        wvalid_buf <= 1'b0;      
end
assign wvalid = wvalid_buf;

//awaddr, wraddr_buf
always @(posedge clk) begin
    if(reset) begin
        wraddr_buf <= 32'b0;
    end
    else if(wr_next_state == `WREQ_START)
        wraddr_buf <= data_sram_addr;
end
assign awaddr = wraddr_buf;

//awsize, wrsize_buf
always @(posedge clk) begin
    if(reset)
        wrsize_buf <= 3'b0;
    else if(wr_next_state == `WREQ_START)
        wrsize_buf <= {1'b0, data_sram_size};
end
assign awsize = wrsize_buf;

//awvalid, wrvalid_buf
always @(posedge clk) begin
    if(reset)
        wrvalid_buf <= 1'b0;
    else if(wr_next_state == `WREQ_AW_VALID || wr_next_state == `WREQ_RET && wr_current_state != `WREQ_AW_VALID)
        wrvalid_buf <= 1'b0;
    else if(wr_next_state == `WREQ_START || wr_next_state == `WREQ_W_VALID)
        wrvalid_buf <= 1'b1;
    else
        wrvalid_buf <= wrvalid_buf;
end    
assign awvalid = wrvalid_buf;

always@(posedge clk) begin
    if(reset) begin
        chg_when_req <= 1'b0;
    end
    else if(inst_sram_req && ((arid == 4'b0) && (araddr_buf != inst_addr_delay))) begin
        chg_when_req <= 1'b1;
    end
    else if(rreq_current_state == `RREQ_RET) begin//原本为current_state
        chg_when_req <= 1'b0;
    end
end
/*************************************************************************************************/
always@(posedge clk) begin
    if(reset) begin
        chg_data_when_req <= 1'b0;
    end
    else if(inst_sram_req && ((arid == 4'b0) && (araddr_buf != inst_addr_delay))) begin
        chg_data_when_req <= 1'b1;
    end
    else if(r_current_state == `R_RET) begin//原来为current_state
        chg_data_when_req <= 1'b0;
    end
end
/***************************************************************************************************/
always@(posedge clk) begin
    if(reset) begin
        rreq_current_state <= `RREQ_RST;
    end
    else begin
        rreq_current_state <= rreq_next_state;
    end
end

always @(posedge clk) begin
    if(reset) begin
        r_current_state <= `R_RST;
        w_current_state <= `W_RST;
    end
    else begin
        r_current_state <= r_next_state;
        w_current_state <= w_next_state;
    end
end


/************************************************FSM*****************************************/
always@(*) begin
    case(rreq_current_state)
        `RREQ_RST: begin
            if(data_sram_req && ~data_sram_wr) begin
                rreq_next_state = `RREQ_BLOCK;
            end
            else if(ex) begin
                rreq_next_state = `RREQ_RST;
            end
            else if(inst_sram_req) begin
                rreq_next_state = `RREQ_INST_VALID;
            end
            else begin
                rreq_next_state = `RREQ_RST;//原本为next_state
            end
        end

        `RREQ_BLOCK: begin
            if(bready) begin
                rreq_next_state = `RREQ_BLOCK; //once there is any write waiting for response, block any read until write successfully.
            end
            /*else if(!data_sram_req) begin
                rreq_next_state = `RREQ_RST;
            end*/
            else begin
                rreq_next_state = `RREQ_DATA_VALID;
            end
        end

        `RREQ_INST_VALID: begin
            if(arvalid && arready) begin
                rreq_next_state = `RREQ_RET;
            end
            /*else if(!inst_sram_req && !arready) begin
                rreq_next_state = `RREQ_RST;
            end*/
            else begin
                rreq_next_state = `RREQ_INST_VALID;
            end
        end

        `RREQ_DATA_VALID: begin
            if(arvalid && arready) begin
                rreq_next_state = `RREQ_RET;
            end
            /*else if(!data_sram_req) begin
                rreq_next_state = `RREQ_RST;
            end*/
            else begin
                rreq_next_state = `RREQ_DATA_VALID;
            end
        end

        `RREQ_RET: begin
            rreq_next_state = `RREQ_RST;
        end

        default: rreq_next_state = `RREQ_RST;
    endcase
end

reg         shaken_buf;
always@(posedge clk) begin
    if(reset) begin
        shaken_buf <= 1'b0;
    end
    else if(arvalid && arready && chg_data_when_req) begin
        shaken_buf <= 1'b1;
    end
    else if(r_next_state == `R_VALID) begin
        shaken_buf <= 1'b0;
    end
    else if(/*r_next_state == `R_VALID*/rready && rvalid) begin
        shaken_buf <= 1'b1;
    end
end

always @(*) begin 
    case(r_current_state)
        `R_RST: begin
            /*if(rreq_current_state == `RREQ_INST_VALID && rreq_next_state == `RREQ_RST && !rvalid) begin
                r_next_state = `R_RST;
            end*/
            if(shaken_buf || arready && arvalid || rready) begin
                r_next_state = `R_VALID;
            end
            else begin
                r_next_state = `R_RST;
            end
        end

        `R_VALID: begin
            if(rvalid && rready) begin
                r_next_state = `R_RET;
            end
            /*else if(rreq_current_state == `RREQ_INST_VALID && rreq_next_state == `RREQ_RST && !rvalid) begin
                r_next_state = `R_RST;
            end*/
            else begin
                r_next_state = `R_VALID;
            end
        end

        `R_RET: begin
            r_next_state = `R_RST;
        end

        default: r_next_state = `R_RST;
    endcase
end

always @(*) begin 
    case(w_current_state)
        `W_RST: begin
            if(wready && wvalid || bready) begin
                w_next_state = `W_VALID;
            end
            else begin
                w_next_state = `W_RST;
            end
        end

        `W_VALID: begin
            if(bvalid && bready) begin
                w_next_state = `W_RET;
            end
            else begin
                w_next_state = `W_VALID;
            end
        end

        `W_RET: begin
            begin
                w_next_state = `W_RST;
            end
        end

        default: w_next_state = `R_RST;
    endcase
end

always @(*) begin
    case(wr_current_state)
        `WREQ_RST: begin
            if(data_sram_req && data_sram_wr)
                wr_next_state = `WREQ_START;
            else
                wr_next_state = `WREQ_RST;
        end
        `WREQ_START: begin
            if(awvalid && awready && wvalid && wready)
                wr_next_state = `WREQ_RET;
            else if(awvalid && awready)
                wr_next_state = `WREQ_AW_VALID;
            else if(wvalid && wready)
                wr_next_state = `WREQ_W_VALID;
            else 
                wr_next_state = `WREQ_START;
        end
        `WREQ_AW_VALID: begin
            if(wvalid && wready)
                wr_next_state = `WREQ_RET;
            else
                wr_next_state = `WREQ_AW_VALID;
        end
        `WREQ_W_VALID: begin
            if(awvalid && awready)
                wr_next_state = `WREQ_RET;
            else
                wr_next_state = `WREQ_W_VALID;
        end
        `WREQ_RET: begin
            wr_next_state = `WREQ_RST;
        end
        default:
            wr_next_state = `WREQ_RST;
    endcase
end

endmodule
