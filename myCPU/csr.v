`define CSR_CRMD        12'h000
`define CSR_PRMD        12'h001
`define CSR_ECFG        12'h004
`define CSR_ESTAT       12'h005
`define CSR_ERA         12'h006
`define CSR_BADV        12'h007
`define CSR_EENTRY      12'h00c
`define CSR_TLBIDX      12'h010
`define CSR_TLBEHI      12'h011
`define CSR_TLBELO0     12'h012
`define CSR_TLBELO1     12'h013
`define CSR_ASID        12'h018
`define CSR_SAVE0       12'h030
`define CSR_SAVE1       12'h031
`define CSR_SAVE2       12'h032
`define CSR_SAVE3       12'h033
`define CSR_TID         12'h040
`define CSR_TCFG        12'h041
`define CSR_TVAL        12'h042
`define CSR_TICLR       12'h044
`define CSR_TLBRENTRY   12'h088
`define CSR_DMW0        12'h180
`define CSR_DMW1        12'h181

`define CSR_CRMD_PLV    1:0
`define CSR_CRMD_IE     2
`define CSR_CRMD_DA     3
`define CSR_CRMD_PG     4
`define CSR_CRMD_DATF   6:5
`define CSR_CRMD_DATM   8:7
`define CSR_PRMD_PPLV   1:0
`define CSR_PRMD_PIE    2
`define CSR_ECFG_LIE    12:0
`define CSR_ESTAT_IS10  1:0
`define CSR_ERA_PC      31:0
`define CSR_EENTRY_VA   31:6
`define CSR_SAVE_DATA   31:0
`define CSR_TID_TID     31:0
`define CSR_TCFG_EN     0
`define CSR_TCFG_PERIOD 1
`define CSR_TCFG_INITV  31:2
`define CSR_TICLR_CLR   0
`define CSR_TLBIDX_INDEX 3:0
`define CSR_TLBIDX_PS   29:24
`define CSR_TLBIDX_NE   31
`define CSR_TLBEHI_VPPN 31:13
`define CSR_TLBELO_D   1
`define CSR_TLBELO_V   0
`define CSR_TLBELO_PLV 3:2
`define CSR_TLBELO_MAT 5:4
`define CSR_TLBELO_G   6
`define CSR_TLBELO_PPN 27:8
`define CSR_ASID_ASID   9:0
`define CSR_ASID_ASIDBITS 23:16
`define CSR_TLBRENTRY_PA 31:6
`define CSR_DMW_PLV0   0
`define CSR_DMW_PLV3   3
`define CSR_DMW_MAT    5:4
`define CSR_DMW_PSEG   27:25
`define CSR_DMW_VSEG   31:29

`define ECODE_ADE 6'h08
`define ECODE_ALE 6'h09

`define ESUBCODE_ADEF 9'h000

module csr(
    input wire         clk, 
    input wire         reset,
    input wire  [31:0] csr_wvalue,
    input wire  [31:0] csr_wmask,
    input wire         csr_we,
    input wire  [11:0] csr_num,
    output wire [31:0] csr_rvalue,

    input wire         wb_ex, //exception signal
    input wire         ertn_flush, //ertn will flush the CRMD
    input wire  [ 5:0] wb_ecode, //for estat ecode
    input wire  [ 8:0] wb_esubcode, //for estat esubcode
    input wire  [31:0] wb_pc, //for era pc
    input wire  [ 7:0] hw_int_in, //hard interrupt, now supposed to be 8'b0
    input wire         ipi_int_in, //interprocessor(?) interrupt, now supposed to be 1'b0
    input wire  [31:0] wb_vaddr, //bad address
    input wire  [31:0] coreid_in, //for resetting tid. supposed to be 32'b0

    output wire        has_int,

    //TLB
    input wire         is_tlbsrch, //if current operating ins is TLBSRCH then set this(maybe you are accessing csr in wb.)
    input wire         is_tlbrd, //if current operating ins is TLBRD then set this
    input wire         tlbsrch_hits, //valid if TLBSRCH hits

    input wire  [ 3:0] tlb_index, //for TLBIDX_index. do TLBSRCH to write
    input wire  [ 5:0] tlb_ps, //for TLBIDX_ps. do TLBRD to write
    input wire         tlb_ne_rd, //for TLBIDX_ne. do TLBRD to write(reverse of the "E" bit of the tlb entry)(REMEMBER TO REVERSE WHEN TLBRD)
    input wire  [18:0] tlb_vppn, //for TLBEHI_vppn. do TLBRD to write
    input wire  [26:0] tlb_lo0_bus, //{PPN, G, MAT, PLV, D, V}. offer this to csr when TLBWR/TLBFILL
    input wire  [26:0] tlb_lo1_bus, //{PPN, G, MAT, PLV, D, V}. offer this to csr when TLBWR/TLBFILL
    //note that g0 and g1 should be similar
    input wire  [ 9:0] tlb_asid, //for ASID_asid. do TLBRD to write

    output wire [ 3:0] tlb_index_r, //take this when you need tlb_index
    output wire [ 5:0] tlb_ps_r, //take this when you need tlb_ps
    output wire        tlb_ne_r, //take this when you need tlb_ne(WHEN TLBWR OR TLBFILL REMEMBER TO REVERSE IN CORE)
    output wire [18:0] tlb_vppn_r, //take this when you need tlb_vppn
    output wire [26:0] tlb_lo0_bus_r, //{PPN, G, MAT, PLV, D, V}
    output wire [26:0] tlb_lo1_bus_r, //{PPN, G, MAT, PLV, D, V}
    output wire [ 9:0] tlb_asid_r, //take this when you need asid_asid
    output wire [ 5:0] ecode,

    //tlb exceptions
    output wire [ 1:0]  crmd_plv_r,
    output wire         crmd_da_r,
    output wire         crmd_pg_r,
    output wire [31:0]  dmw0_r,
    output wire [31:0]  dmw1_r

);

//all reserved seen as 0 when outputing.

//CRMD
reg [ 1:0] csr_crmd_plv;
reg        csr_crmd_ie;
reg        csr_crmd_da;
reg        csr_crmd_pg;
reg [ 1:0] csr_crmd_datf;
reg [ 1:0] csr_crmd_datm;
reg [22:0] csr_crmd_reserved;
//CRMD

//PRMD
reg [ 1:0] csr_prmd_pplv;
reg        csr_prmd_pie;
reg [28:0] csr_prmd_reserved;
//PRMD

//ECFG
reg [12:0] csr_ecfg_lie;
reg [18:0] csr_ecfg_reserved;
//ECFG

//ESTAT
reg [12:0] csr_estat_is;
reg [ 2:0] csr_estat_reserved_15_13;
reg [ 5:0] csr_estat_ecode;
reg [ 8:0] csr_estat_esubcode;
reg        csr_estat_reserved_31;
//ESTAT

//ERA
reg [31:0] csr_era_pc;
//ERA

//BADV
reg [31:0] csr_badv_vaddr;
//BADV

//EENTRY
reg [25:0] csr_eentry_va;
reg [ 5:0] csr_eentry_reserved;
//EENTRY

//SAVE
reg [31:0] csr_save0_data;
reg [31:0] csr_save1_data;
reg [31:0] csr_save2_data;
reg [31:0] csr_save3_data;
//SAVE

//TID
reg [31:0] csr_tid_tid;
//TID

//TCFG
reg        csr_tcfg_en;
reg        csr_tcfg_periodic;
reg [29:0] csr_tcfg_initval; //I'm not sure if 30bits is adequate.
//TCFG

//TVAL
wire [31:0] csr_tval;
//TVAL

//TICLR
wire       csr_ticlr_clr;
//TICLR

//TLBIDX
reg [ 3:0] csr_tlbidx_index;
reg [11:0] csr_tlbidx_reserved_15_4;
reg [ 7:0] csr_tlbidx_reserved_23_16;
reg [ 5:0] csr_tlbidx_ps;
reg        csr_tlbidx_reserved_30;
reg        csr_tlbidx_ne;
//TLBIDX

//TLBEHI
reg [12:0] csr_tlbehi_reserved;
reg [18:0] csr_tlbehi_vppn;
//TLBEHI

//TLBELO0
reg        csr_tlbelo0_v;
reg        csr_tlbelo0_d;
reg [ 1:0] csr_tlbelo0_plv;
reg [ 1:0] csr_tlbelo0_mat;
reg        csr_tlbelo0_g;
reg        csr_tlbelo0_reserved_7;
reg [19:0] csr_tlbelo0_ppn;
reg [ 3:0] csr_tlbelo0_reserved_31_28;
//TLBELO0

//TLBELO1
reg        csr_tlbelo1_v;
reg        csr_tlbelo1_d;
reg [ 1:0] csr_tlbelo1_plv;
reg [ 1:0] csr_tlbelo1_mat;
reg        csr_tlbelo1_g;
reg        csr_tlbelo1_reserved_7;
reg [19:0] csr_tlbelo1_ppn;
reg [ 3:0] csr_tlbelo1_reserved_31_28;
//TLBELO1

//ASID
reg [ 9:0] csr_asid_asid;
reg [ 5:0] csr_asid_reserved_15_10;
reg [ 7:0] csr_asid_asidbits; //=0xa
reg [ 7:0] csr_asid_reserved_31_24;
//ASID

//TLBRENTRY
reg [ 5:0] csr_tlbrentry_reserved;
reg [25:0] csr_tlbrentry_pa;
//TLBRENTRY

//DMW0
reg        csr_dmw0_plv0;
reg [ 1:0] csr_dmw0_reserved_2_1;
reg        csr_dmw0_plv3;
reg [ 1:0] csr_dmw0_mat;
reg [18:0] csr_dmw0_reserved_24_6;
reg [ 2:0] csr_dmw0_pseg;
reg        csr_dmw0_reserved_28;
reg [ 2:0] csr_dmw0_vseg;
//DMW0

//DMW1
reg        csr_dmw1_plv0;
reg [ 1:0] csr_dmw1_reserved_2_1;
reg        csr_dmw1_plv3;
reg [ 1:0] csr_dmw1_mat;
reg [18:0] csr_dmw1_reserved_24_6;
reg [ 2:0] csr_dmw1_pseg;
reg        csr_dmw1_reserved_28;
reg [ 2:0] csr_dmw1_vseg;
//DMW1

reg [31:0] timer_cnt;

always @(posedge clk) begin
    if (reset) begin
        csr_crmd_plv <= 2'b0;
        csr_crmd_da <= 1'b1;
        csr_crmd_pg <= 1'b0;
        csr_crmd_datf <= 2'b0;
        csr_crmd_datm <= 2'b0;
    end
    else if (wb_ex) begin
        csr_crmd_plv <= 2'b0;
        if(wb_ecode == 6'h3f) begin
            csr_crmd_da <= 1'b1;
            csr_crmd_pg <= 1'b0;
        end
    end
    else if (ertn_flush) begin
        csr_crmd_plv <= csr_prmd_pplv;
        if(csr_estat_ecode == 6'h3f) begin
            csr_crmd_da <= 1'b0;
            csr_crmd_pg <= 1'b1;
        end
    end
    else if (csr_we && csr_num ==`CSR_CRMD) begin
        csr_crmd_plv <= csr_wmask[`CSR_CRMD_PLV]&csr_wvalue[`CSR_CRMD_PLV]
        | ~csr_wmask[`CSR_CRMD_PLV]&csr_crmd_plv;
        csr_crmd_da <= csr_wmask[`CSR_CRMD_DA]&csr_wvalue[`CSR_CRMD_DA]
        | ~csr_wmask[`CSR_CRMD_DA]&csr_crmd_da;
        csr_crmd_pg <= csr_wmask[`CSR_CRMD_PG]&csr_wvalue[`CSR_CRMD_PG]
        | ~csr_wmask[`CSR_CRMD_PG]&csr_crmd_pg;
        csr_crmd_datf <= csr_wmask[`CSR_CRMD_DATF]&csr_wvalue[`CSR_CRMD_DATF]
        | ~csr_wmask[`CSR_CRMD_DATF]&csr_crmd_datf;
        csr_crmd_datm <= csr_wmask[`CSR_CRMD_DATM]&csr_wvalue[`CSR_CRMD_DATM]
        | ~csr_wmask[`CSR_CRMD_DATM]&csr_crmd_datm;
    end
end
assign crmd_plv_r = csr_crmd_plv;
assign crmd_da_r = csr_crmd_da;
assign crmd_pg_r = csr_crmd_pg;

always @(posedge clk) begin
    if (reset)
        csr_crmd_ie <= 1'b0;
    else if (wb_ex)
        csr_crmd_ie <= 1'b0;
    else if (ertn_flush)
        csr_crmd_ie <= csr_prmd_pie;
    else if (csr_we && csr_num==`CSR_CRMD)
        csr_crmd_ie <= csr_wmask[`CSR_CRMD_IE]&csr_wvalue[`CSR_CRMD_IE]
        | ~csr_wmask[`CSR_CRMD_IE]&csr_crmd_ie;
end

always @(posedge clk) begin
    if (wb_ex) begin
        csr_prmd_pplv <= csr_crmd_plv;
        csr_prmd_pie <= csr_crmd_ie;
    end
    else if (csr_we && csr_num==`CSR_PRMD) begin
        csr_prmd_pplv <= csr_wmask[`CSR_PRMD_PPLV]&csr_wvalue[`CSR_PRMD_PPLV]
        | ~csr_wmask[`CSR_PRMD_PPLV]&csr_prmd_pplv;
        csr_prmd_pie <= csr_wmask[`CSR_PRMD_PIE]&csr_wvalue[`CSR_PRMD_PIE]
        | ~csr_wmask[`CSR_PRMD_PIE]&csr_prmd_pie;
    end
end

always @(posedge clk) begin
    if (reset)
        csr_ecfg_lie <= 13'b0;
    else if (csr_we && csr_num==`CSR_ECFG)
        csr_ecfg_lie <= csr_wmask[`CSR_ECFG_LIE]&csr_wvalue[`CSR_ECFG_LIE]
        | ~csr_wmask[`CSR_ECFG_LIE]&csr_ecfg_lie;
end

always @(posedge clk) begin
    if (reset)
        csr_estat_is[1:0] <= 2'b0;
    else if (csr_we && csr_num==`CSR_ESTAT)
        csr_estat_is[1:0] <= csr_wmask[`CSR_ESTAT_IS10]&csr_wvalue[`CSR_ESTAT_IS10]
        | ~csr_wmask[`CSR_ESTAT_IS10]&csr_estat_is[1:0];
        csr_estat_is[9:2] <= hw_int_in[7:0];
        csr_estat_is[10] <= 1'b0;
    if (timer_cnt[31:0]==32'b0)
        csr_estat_is[11] <= 1'b1;
    else if (csr_we && csr_num==`CSR_TICLR && csr_wmask[`CSR_TICLR_CLR]
             && csr_wvalue[`CSR_TICLR_CLR])
        csr_estat_is[11] <= 1'b0;
        csr_estat_is[12] <= ipi_int_in;
end

always @(posedge clk) begin
    if (wb_ex) begin
        csr_estat_ecode <= wb_ecode;
        csr_estat_esubcode <= wb_esubcode;
    end
end

assign ecode = csr_estat_ecode;

always @(posedge clk) begin
    if (wb_ex)
        csr_era_pc <= wb_pc;
    else if (csr_we && csr_num==`CSR_ERA)
        csr_era_pc <= csr_wmask[`CSR_ERA_PC]&csr_wvalue[`CSR_ERA_PC]
        | ~csr_wmask[`CSR_ERA_PC]&csr_era_pc;
end

always @(posedge clk) begin
    if (csr_we && csr_num==`CSR_EENTRY)
        csr_eentry_va <= csr_wmask[`CSR_EENTRY_VA]&csr_wvalue[`CSR_EENTRY_VA]
        | ~csr_wmask[`CSR_EENTRY_VA]&csr_eentry_va;
end

always @(posedge clk) begin
    if (csr_we && csr_num==`CSR_SAVE0)
        csr_save0_data <= csr_wmask[`CSR_SAVE_DATA]&csr_wvalue[`CSR_SAVE_DATA]
        | ~csr_wmask[`CSR_SAVE_DATA]&csr_save0_data;
    if (csr_we && csr_num==`CSR_SAVE1)
        csr_save1_data <= csr_wmask[`CSR_SAVE_DATA]&csr_wvalue[`CSR_SAVE_DATA]
        | ~csr_wmask[`CSR_SAVE_DATA]&csr_save1_data;
    if (csr_we && csr_num==`CSR_SAVE2)
        csr_save2_data <= csr_wmask[`CSR_SAVE_DATA]&csr_wvalue[`CSR_SAVE_DATA]
        | ~csr_wmask[`CSR_SAVE_DATA]&csr_save2_data;
    if (csr_we && csr_num==`CSR_SAVE3)
        csr_save3_data <= csr_wmask[`CSR_SAVE_DATA]&csr_wvalue[`CSR_SAVE_DATA]
        | ~csr_wmask[`CSR_SAVE_DATA]&csr_save3_data;
end

wire      wb_ex_addr_err;
assign wb_ex_addr_err = wb_ecode == 6'h01 || wb_ecode == 6'h02 || wb_ecode == 6'h03 || wb_ecode == 6'h04 || wb_ecode == 6'h07 || wb_ecode == 6'h3f || wb_ecode == 6'h08 || wb_ecode == 6'h09;

always @(posedge clk) begin
    if (wb_ex && wb_ex_addr_err)
        csr_badv_vaddr <= (wb_ecode==`ECODE_ADE &&
        wb_esubcode==`ESUBCODE_ADEF) ? wb_pc : wb_vaddr;
end

always @(posedge clk) begin
    if (reset)
        csr_tid_tid <= coreid_in;
    else if (csr_we && csr_num==`CSR_TID)
        csr_tid_tid <= csr_wmask[`CSR_TID_TID]&csr_wvalue[`CSR_TID_TID]
        | ~csr_wmask[`CSR_TID_TID]&csr_tid_tid;
end

always @(posedge clk) begin
    if (reset)
        csr_tcfg_en <= 1'b0;
    else if (csr_we && csr_num==`CSR_TCFG)
        csr_tcfg_en <= csr_wmask[`CSR_TCFG_EN]&csr_wvalue[`CSR_TCFG_EN]
        | ~csr_wmask[`CSR_TCFG_EN]&csr_tcfg_en;
    if (csr_we && csr_num==`CSR_TCFG) begin
        csr_tcfg_periodic <= csr_wmask[`CSR_TCFG_PERIOD]&csr_wvalue[`CSR_TCFG_PERIOD]
        | ~csr_wmask[`CSR_TCFG_PERIOD]&csr_tcfg_periodic;
        csr_tcfg_initval <= csr_wmask[`CSR_TCFG_INITV]&csr_wvalue[`CSR_TCFG_INITV]
        | ~csr_wmask[`CSR_TCFG_INITV]&csr_tcfg_initval;
    end
end

wire [31:0] tcfg_next_value;

assign tcfg_next_value = csr_wmask[31:0]&csr_wvalue[31:0] | ~csr_wmask[31:0] & {csr_tcfg_initval, csr_tcfg_periodic, csr_tcfg_en};

always @(posedge clk) begin
    if (reset)
        timer_cnt <= 32'hffffffff;
    else if (csr_we && csr_num==`CSR_TCFG && tcfg_next_value[`CSR_TCFG_EN])
        timer_cnt <= {tcfg_next_value[`CSR_TCFG_INITV], 2'b0};
    else if (csr_tcfg_en && timer_cnt!=32'hffffffff) begin
        if (timer_cnt[31:0]==32'b0 && csr_tcfg_periodic)
            timer_cnt <= {csr_tcfg_initval, 2'b0};
        else
            timer_cnt <= timer_cnt - 1'b1;
    end
end
assign csr_tval = timer_cnt[31:0];

assign csr_ticlr_clr = 1'b0;

//TLBIDX
always@(posedge clk) begin
    if(is_tlbsrch) begin
        if(tlbsrch_hits) begin
            csr_tlbidx_index <= tlb_index;
            csr_tlbidx_ne <= 1'b0;
        end
        else begin
            csr_tlbidx_ne <= 1'b1;
        end
    end
    else if(is_tlbrd) begin 
        csr_tlbidx_ps <= tlb_ps;
        csr_tlbidx_ne <= tlb_ne_rd;
    end
    else if(csr_we && csr_num == `CSR_TLBIDX) begin
        csr_tlbidx_index <= csr_wmask[`CSR_TLBIDX_INDEX]&csr_wvalue[`CSR_TLBIDX_INDEX]
        | ~csr_wmask[`CSR_TLBIDX_INDEX]&csr_tlbidx_index;
        csr_tlbidx_ps    <= csr_wmask[`CSR_TLBIDX_PS]&csr_wvalue[`CSR_TLBIDX_PS]
        | ~csr_wmask[`CSR_TLBIDX_PS]&csr_tlbidx_ps;
        csr_tlbidx_ne    <= csr_wmask[`CSR_TLBIDX_NE]&csr_wvalue[`CSR_TLBIDX_NE]
        | ~csr_wmask[`CSR_TLBIDX_NE]&csr_tlbidx_ne;
    end
end
assign tlb_index_r = csr_tlbidx_index;
assign tlb_ps_r = csr_tlbidx_ps;
assign tlb_ne_r = csr_tlbidx_ne;

//TLBEHI
always@(posedge clk) begin 
    if(wb_ecode == 6'h01 || wb_ecode == 6'h02 || wb_ecode == 6'h03 || wb_ecode == 6'h04 || wb_ecode == 6'h07 || wb_ecode == 6'h3f) begin
        csr_tlbehi_vppn <= wb_vaddr[31:13];
    end
    else if(is_tlbrd) begin
        csr_tlbehi_vppn <= tlb_vppn;
    end
    else if(csr_we && csr_num == `CSR_TLBEHI) begin 
        csr_tlbehi_vppn <= csr_wmask[`CSR_TLBEHI_VPPN]&csr_wvalue[`CSR_TLBEHI_VPPN]
        | ~csr_wmask[`CSR_TLBEHI_VPPN]&csr_tlbehi_vppn;
    end
end
assign tlb_vppn_r = csr_tlbehi_vppn;

//TLBELO0
wire        v0;
wire        d0;
wire [ 1:0] plv0;
wire [ 1:0] mat0;
wire        g0;
wire [19:0] ppn0;
assign {ppn0, g0, mat0, plv0, d0, v0} = tlb_lo0_bus;
always@(posedge clk) begin
    if(is_tlbrd) begin 
        csr_tlbelo0_g <= g0;
        csr_tlbelo0_v <= v0;
        csr_tlbelo0_d <= d0;
        csr_tlbelo0_plv <= plv0;
        csr_tlbelo0_mat <= mat0;
        csr_tlbelo0_ppn <= ppn0;
    end
    else if(csr_we && csr_num == `CSR_TLBELO0) begin 
        csr_tlbelo0_d <= csr_wmask[`CSR_TLBELO_D]&csr_wvalue[`CSR_TLBELO_D]
        | ~csr_wmask[`CSR_TLBELO_D]&csr_tlbelo0_d;
        csr_tlbelo0_v <= csr_wmask[`CSR_TLBELO_V]&csr_wvalue[`CSR_TLBELO_V]
        | ~csr_wmask[`CSR_TLBELO_V]&csr_tlbelo0_v;
        csr_tlbelo0_plv <= csr_wmask[`CSR_TLBELO_PLV]&csr_wvalue[`CSR_TLBELO_PLV]
        | ~csr_wmask[`CSR_TLBELO_PLV]&csr_tlbelo0_plv;
        csr_tlbelo0_mat <= csr_wmask[`CSR_TLBELO_MAT]&csr_wvalue[`CSR_TLBELO_MAT]
        | ~csr_wmask[`CSR_TLBELO_MAT]&csr_tlbelo0_mat;
        csr_tlbelo0_g <= csr_wmask[`CSR_TLBELO_G]&csr_wvalue[`CSR_TLBELO_G]
        | ~csr_wmask[`CSR_TLBELO_G]&csr_tlbelo0_g;
        csr_tlbelo0_ppn <= csr_wmask[`CSR_TLBELO_PPN]&csr_wvalue[`CSR_TLBELO_PPN]
        | ~csr_wmask[`CSR_TLBELO_PPN]&csr_tlbelo0_ppn;
    end
end
assign tlb_lo0_bus_r = {csr_tlbelo0_ppn, csr_tlbelo0_g, csr_tlbelo0_mat, csr_tlbelo0_plv, csr_tlbelo0_d, csr_tlbelo0_v};

//TLBELO1
wire        v1;
wire        d1;
wire [ 1:0] plv1;
wire [ 1:0] mat1;
wire        g1;
wire [19:0] ppn1;
assign {ppn1, g1, mat1, plv1, d1, v1} = tlb_lo1_bus;
always@(posedge clk) begin
    if(is_tlbrd) begin 
        csr_tlbelo1_g <= g1;
        csr_tlbelo1_v <= v1;
        csr_tlbelo1_d <= d1;
        csr_tlbelo1_plv <= plv1;
        csr_tlbelo1_mat <= mat1;
        csr_tlbelo1_ppn <= ppn1;
    end
    else if(csr_we && csr_num == `CSR_TLBELO1) begin 
        csr_tlbelo1_d <= csr_wmask[`CSR_TLBELO_D]&csr_wvalue[`CSR_TLBELO_D]
        | ~csr_wmask[`CSR_TLBELO_D]&csr_tlbelo1_d;
        csr_tlbelo1_v <= csr_wmask[`CSR_TLBELO_V]&csr_wvalue[`CSR_TLBELO_V]
        | ~csr_wmask[`CSR_TLBELO_V]&csr_tlbelo1_v;
        csr_tlbelo1_plv <= csr_wmask[`CSR_TLBELO_PLV]&csr_wvalue[`CSR_TLBELO_PLV]
        | ~csr_wmask[`CSR_TLBELO_PLV]&csr_tlbelo1_plv;
        csr_tlbelo1_mat <= csr_wmask[`CSR_TLBELO_MAT]&csr_wvalue[`CSR_TLBELO_MAT]
        | ~csr_wmask[`CSR_TLBELO_MAT]&csr_tlbelo1_mat;
        csr_tlbelo1_g <= csr_wmask[`CSR_TLBELO_G]&csr_wvalue[`CSR_TLBELO_G]
        | ~csr_wmask[`CSR_TLBELO_G]&csr_tlbelo1_g;
        csr_tlbelo1_ppn <= csr_wmask[`CSR_TLBELO_PPN]&csr_wvalue[`CSR_TLBELO_PPN]
        | ~csr_wmask[`CSR_TLBELO_PPN]&csr_tlbelo1_ppn;
    end
end
assign tlb_lo1_bus_r = {csr_tlbelo1_ppn, csr_tlbelo1_g, csr_tlbelo1_mat, csr_tlbelo1_plv, csr_tlbelo1_d, csr_tlbelo1_v};

//ASID
always@(posedge clk) begin 
    if(is_tlbrd) begin 
        csr_asid_asid <= tlb_asid;
    end
    else if(csr_we && csr_num == `CSR_ASID) begin 
        csr_asid_asid <= csr_wmask[`CSR_ASID_ASID]&csr_wvalue[`CSR_ASID_ASID]
        | ~csr_wmask[`CSR_ASID_ASID]&csr_asid_asid;
    end
end
assign tlb_asid_r = csr_asid_asid;

//TLBRENTRY
always@(posedge clk) begin 
    if(csr_we && csr_num == `CSR_TLBRENTRY) begin 
        csr_tlbrentry_pa <= csr_wmask[`CSR_TLBRENTRY_PA]&csr_wvalue[`CSR_TLBRENTRY_PA]
        | ~csr_wmask[`CSR_TLBRENTRY_PA]&csr_tlbrentry_pa;
    end
end
//when ex_refill, you should set csr_num to `CSR_TLBRENTRY to read this csr.

//DMW0
always@(posedge clk) begin
    if(reset) begin
        csr_dmw0_plv0 <= 1'b0;
        csr_dmw0_plv3 <= 1'b0;
        csr_dmw0_vseg <= 3'b0;
        csr_dmw0_pseg <= 3'b0;
        csr_dmw0_mat  <= 2'b0;
    end
    else if(csr_we && csr_num == `CSR_DMW0) begin 
        csr_dmw0_plv0 <= csr_wmask[`CSR_DMW_PLV0]&csr_wvalue[`CSR_DMW_PLV0]
        | ~csr_wmask[`CSR_DMW_PLV0]&csr_dmw0_plv0;
        csr_dmw0_plv3 <= csr_wmask[`CSR_DMW_PLV3]&csr_wvalue[`CSR_DMW_PLV3]
        | ~csr_wmask[`CSR_DMW_PLV3]&csr_dmw0_plv3;
        csr_dmw0_mat <= csr_wmask[`CSR_DMW_MAT]&csr_wvalue[`CSR_DMW_MAT]
        | ~csr_wmask[`CSR_DMW_MAT]&csr_dmw0_mat;
        csr_dmw0_pseg <= csr_wmask[`CSR_DMW_PSEG]&csr_wvalue[`CSR_DMW_PSEG]
        | ~csr_wmask[`CSR_DMW_PSEG]&csr_dmw0_pseg;
        csr_dmw0_vseg <= csr_wmask[`CSR_DMW_VSEG]&csr_wvalue[`CSR_DMW_VSEG]
        | ~csr_wmask[`CSR_DMW_VSEG]&csr_dmw0_vseg;
    end
end

//DMW1
always@(posedge clk) begin
    if(reset) begin
        csr_dmw1_plv0 <= 1'b0;
        csr_dmw1_plv3 <= 1'b0;
        csr_dmw1_vseg <= 3'b0;
        csr_dmw1_pseg <= 3'b0;
        csr_dmw1_mat  <= 2'b0;
    end
    else if(csr_we && csr_num == `CSR_DMW1) begin 
        csr_dmw1_plv0 <= csr_wmask[`CSR_DMW_PLV0]&csr_wvalue[`CSR_DMW_PLV0]
        | ~csr_wmask[`CSR_DMW_PLV0]&csr_dmw1_plv0;
        csr_dmw1_plv3 <= csr_wmask[`CSR_DMW_PLV3]&csr_wvalue[`CSR_DMW_PLV3]
        | ~csr_wmask[`CSR_DMW_PLV3]&csr_dmw1_plv3;
        csr_dmw1_mat <= csr_wmask[`CSR_DMW_MAT]&csr_wvalue[`CSR_DMW_MAT]
        | ~csr_wmask[`CSR_DMW_MAT]&csr_dmw1_mat;
        csr_dmw1_pseg <= csr_wmask[`CSR_DMW_PSEG]&csr_wvalue[`CSR_DMW_PSEG]
        | ~csr_wmask[`CSR_DMW_PSEG]&csr_dmw1_pseg;
        csr_dmw1_vseg <= csr_wmask[`CSR_DMW_VSEG]&csr_wvalue[`CSR_DMW_VSEG]
        | ~csr_wmask[`CSR_DMW_VSEG]&csr_dmw1_vseg;
    end
end

wire [31:0] csr_crmd_rvalue;
wire [31:0] csr_prmd_rvalue;
wire [31:0] csr_ecfg_rvalue;
wire [31:0] csr_estat_rvalue;
wire [31:0] csr_era_rvalue;
wire [31:0] csr_eentry_rvalue;
wire [31:0] csr_save0_rvalue;
wire [31:0] csr_save1_rvalue;
wire [31:0] csr_save2_rvalue;
wire [31:0] csr_save3_rvalue;
wire [31:0] csr_badv_rvalue;
wire [31:0] csr_tid_rvalue;
wire [31:0] csr_tcfg_rvalue;
wire [31:0] csr_tval_rvalue;
wire [31:0] csr_ticlr_rvalue;
wire [31:0] csr_tlbidx_rvalue;
wire [31:0] csr_tlbehi_rvalue;
wire [31:0] csr_tlbelo0_rvalue;
wire [31:0] csr_tlbelo1_rvalue;
wire [31:0] csr_asid_rvalue;
wire [31:0] csr_tlbrentry_rvalue;
wire [31:0] csr_dmw0_rvalue;
wire [31:0] csr_dmw1_rvalue;

assign csr_crmd_rvalue   = {23'b0, csr_crmd_datm, csr_crmd_datf, csr_crmd_pg, csr_crmd_da, csr_crmd_ie, csr_crmd_plv};
assign csr_prmd_rvalue   = {29'b0, csr_prmd_pie, csr_prmd_pplv};
assign csr_ecfg_rvalue   = {19'b0, csr_ecfg_lie};
assign csr_estat_rvalue  = {1'b0, csr_estat_esubcode, csr_estat_ecode, 3'b0, csr_estat_is};
assign csr_era_rvalue    = csr_era_pc;
assign csr_eentry_rvalue = {csr_eentry_va, 6'b0};
assign csr_save0_rvalue  = csr_save0_data;
assign csr_save1_rvalue  = csr_save1_data;
assign csr_save2_rvalue  = csr_save2_data;
assign csr_save3_rvalue  = csr_save3_data;
assign csr_badv_rvalue   = csr_badv_vaddr;
assign csr_tid_rvalue    = csr_tid_tid;
assign csr_tcfg_rvalue   = {csr_tcfg_initval, csr_tcfg_periodic, csr_tcfg_en};
assign csr_tval_rvalue   = csr_tval;
assign csr_ticlr_rvalue  = {31'b0, csr_ticlr_clr};
assign csr_tlbidx_rvalue = {csr_tlbidx_ne, 1'b0, csr_tlbidx_ps, 8'b0, 12'b0, csr_tlbidx_index};
assign csr_tlbehi_rvalue = {csr_tlbehi_vppn, 13'b0};
assign csr_tlbelo0_rvalue = {4'b0, csr_tlbelo0_ppn, 1'b0, csr_tlbelo0_g, csr_tlbelo0_mat, csr_tlbelo0_plv, csr_tlbelo0_d, csr_tlbelo0_v};
assign csr_tlbelo1_rvalue = {4'b0, csr_tlbelo1_ppn, 1'b0, csr_tlbelo1_g, csr_tlbelo1_mat, csr_tlbelo1_plv, csr_tlbelo1_d, csr_tlbelo1_v};
assign csr_asid_rvalue   = {8'b0, 8'h0a, 6'b0, csr_asid_asid};
assign csr_tlbrentry_rvalue = {csr_tlbrentry_pa, 6'b0};
assign csr_dmw0_rvalue   = {csr_dmw0_vseg, 1'b0, csr_dmw0_pseg, 19'b0, csr_dmw0_mat, csr_dmw0_plv3, 2'b0, csr_dmw0_plv0};
assign csr_dmw1_rvalue   = {csr_dmw1_vseg, 1'b0, csr_dmw1_pseg, 19'b0, csr_dmw1_mat, csr_dmw1_plv3, 2'b0, csr_dmw1_plv0};

assign dmw0_r = csr_dmw0_rvalue;
assign dmw1_r = csr_dmw1_rvalue;

assign csr_rvalue = {32{csr_num == `CSR_CRMD}} & csr_crmd_rvalue
                    | {32{csr_num == `CSR_PRMD}} & csr_prmd_rvalue
                    | {32{csr_num == `CSR_ECFG}} & csr_ecfg_rvalue
                    | {32{csr_num == `CSR_ESTAT}} & csr_estat_rvalue
                    | {32{csr_num == `CSR_ERA}} & csr_era_rvalue
                    | {32{csr_num == `CSR_EENTRY}} & csr_eentry_rvalue
                    | {32{csr_num == `CSR_SAVE0}} & csr_save0_rvalue
                    | {32{csr_num == `CSR_SAVE1}} & csr_save1_rvalue
                    | {32{csr_num == `CSR_SAVE2}} & csr_save2_rvalue
                    | {32{csr_num == `CSR_SAVE3}} & csr_save3_rvalue
                    | {32{csr_num == `CSR_BADV}} & csr_badv_rvalue
                    | {32{csr_num == `CSR_TID}} & csr_tid_rvalue
                    | {32{csr_num == `CSR_TCFG}} & csr_tcfg_rvalue
                    | {32{csr_num == `CSR_TVAL}} & csr_tval_rvalue
                    | {32{csr_num == `CSR_TICLR}} & csr_ticlr_rvalue
                    | {32{csr_num == `CSR_TLBIDX}} & csr_tlbidx_rvalue
                    | {32{csr_num == `CSR_TLBEHI}} & csr_tlbehi_rvalue
                    | {32{csr_num == `CSR_TLBELO0}} & csr_tlbelo0_rvalue
                    | {32{csr_num == `CSR_TLBELO1}} & csr_tlbelo1_rvalue
                    | {32{csr_num == `CSR_ASID}} & csr_asid_rvalue
                    | {32{csr_num == `CSR_TLBRENTRY}} & csr_tlbrentry_rvalue
                    | {32{csr_num == `CSR_DMW0}} & csr_dmw0_rvalue
                    | {32{csr_num == `CSR_DMW1}} & csr_dmw1_rvalue;

assign has_int = ((csr_estat_is[11:0] & csr_ecfg_lie[11:0]) != 12'b0) && (csr_crmd_ie == 1'b1);

endmodule