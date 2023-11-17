module mycpu_top(
    input wire      aclk,
    input wire      aresetn,

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
    input wire [ 3:0] rid,
    input wire [31:0] rdata,
    input wire [ 1:0] rresp,
    input wire        rlast,
    input wire        rvalid,
    output wire        rready,

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
    input wire [ 3:0] bid,
    input wire [ 1:0] bresp,
    input wire        bvalid,
    output wire        bready,

    // trace debug interface
    output wire [31:0] debug_wb_pc,
    output wire [ 3:0] debug_wb_rf_we,
    output wire [ 4:0] debug_wb_rf_wnum,
    output wire [31:0] debug_wb_rf_wdata
);
wire         reset;
assign reset = ~aresetn;

wire        inst_sram_req;
wire        inst_sram_wr;
wire [ 1:0] inst_sram_size;
wire [31:0] inst_sram_addr;
wire [ 3:0] inst_sram_wstrb;
wire [31:0] inst_sram_wdata;
wire        inst_sram_addr_ok;
wire        inst_sram_data_ok;
wire [31:0] inst_sram_rdata;

wire        data_sram_req;
wire        data_sram_wr;
wire [ 1:0] data_sram_size;
wire [31:0] data_sram_addr;
wire [ 3:0] data_sram_wstrb;
wire [31:0] data_sram_wdata;
wire        data_sram_addr_ok;
wire        data_sram_data_ok;
wire [31:0] data_sram_rdata;

wire [31:0] seq_pc;
wire [31:0] nextpc;
wire        br_taken;
wire [31:0] br_target;
wire        br_stall;

wire        rj_eq_rd;
wire        rj_lt_rd;
wire        rj_lt_rd_u;

wire [18:0] alu_op;
wire        load_op;
wire        src1_is_pc;
wire        src2_is_imm;
wire        res_from_mem;
wire        dst_is_r1;
wire        gr_we;
wire        src_reg_is_rd;
wire        mem_we;
wire [4: 0] dest;
wire [31:0] rj_value;
wire [31:0] rkd_value;
wire [31:0] imm;
wire [31:0] br_offs;
wire [31:0] jirl_offs;

wire [ 5:0] op_31_26;
wire [ 3:0] op_25_22;
wire [ 1:0] op_21_20;
wire [ 4:0] op_19_15;
wire [ 4:0] rd;
wire [ 4:0] rj;
wire [ 4:0] rk;
wire [11:0] i12;
wire [19:0] i20;
wire [15:0] i16;
wire [25:0] i26;

wire [63:0] op_31_26_d;
wire [15:0] op_25_22_d;
wire [ 3:0] op_21_20_d;
wire [31:0] op_19_15_d;

wire        inst_add_w;
wire        inst_sub_w;
wire        inst_slt;
wire        inst_sltu;
wire        inst_nor;
wire        inst_and;
wire        inst_or;
wire        inst_xor;
wire        inst_slli_w;
wire        inst_srli_w;
wire        inst_srai_w;
wire        inst_addi_w;
wire        inst_ld_w;
wire        inst_st_w;
wire        inst_jirl;
wire        inst_b;
wire        inst_bl;
wire        inst_beq;
wire        inst_bne;
wire        inst_lu12i_w;

//exp10
wire        inst_slti;
wire        inst_sltui;
wire        inst_andi;
wire        inst_ori;
wire        inst_xori;
wire        inst_sll_w;
wire        inst_srl_w;
wire        inst_sra_w;
wire        inst_pcaddu12i;
wire        inst_mul_w;
wire        inst_mulh_w;
wire        inst_mulh_wu;
wire        inst_div_w;
wire        inst_mod_w;
wire        inst_div_wu;
wire        inst_mod_wu;
//exp10

//exp11
wire        inst_blt;
wire        inst_bge;
wire        inst_bltu;
wire        inst_bgeu;
wire        inst_ld_b;
wire        inst_ld_h;
wire        inst_ld_bu;
wire        inst_ld_hu;
wire        inst_st_b;
wire        inst_st_h;
//exp11

//exp12 & exp13
wire        inst_csrrd;
wire        inst_csrwr;
wire        inst_csrxchg;
wire        inst_ertn;
wire        inst_syscall;
wire        inst_break;

wire        inst_rdcntvl_w;
wire        inst_rdcntvh_w;
wire        inst_rdcntid;
//exp12 & exp13

//exp18
wire        inst_invtlb;
wire        inst_tlbsrch;
wire        inst_tlbrd;
wire        inst_tlbwr;
wire        inst_tlbfill;
//exp18

//exp19
wire [2:0] if_addr_mode;
wire [2:0] mem_addr_mode;

wire [31:0] if_tlb_addr;
wire [31:0] mem_tlb_addr;
//exp19
 
wire [4:0] ld_what;
wire [2:0] st_what;
wire [31:0] final_inst;

wire        need_ui5;
wire        need_si12;
wire        need_ui12;
wire        need_si16;
wire        need_si20;
wire        need_si26;
wire        src2_is_4;

wire [ 4:0] rf_raddr1;
wire [31:0] rf_rdata1;
wire [ 4:0] rf_raddr2;
wire [31:0] rf_rdata2;
wire        rf_we   ;
wire [ 4:0] rf_waddr;
wire [31:0] rf_wdata;

wire [31:0] alu_src1   ;
wire [31:0] alu_src2   ;
wire [31:0] alu_result ;
wire        alu_ready  ;

wire        tlb_block;
wire [31:0] mem_result;
wire [31:0] final_result;

wire        ertn_flush;
wire        exc_syscall;
wire        exc_break;
wire        exc_ine;
wire        exc_ale;
wire        exc_ppi_f;
wire        exc_tlbr_f;
wire        exc_ppi_m;
wire        exc_tlbr_m;
wire        exc_pis;
wire        exc_pil;
wire        exc_pif;
wire        exc_pme;
wire        exc_adem;

//IF reg
reg [31:0] pc;
reg        if_valid;
reg        if_ex;
reg        if_exc_adef;
reg        if_exc_ppi;
reg        if_exc_tlbr;
reg        if_exc_pif;
reg [31:0] if_badv;

//ID reg
reg        id_valid;
reg [31:0] id_pc;
reg [31:0] inst;
reg        id_ex;
reg        id_exc_adef;
reg        id_exc_ppi;
reg        id_exc_tlbr;
reg        id_exc_pif;
reg [31:0] id_badv;
reg        id_need_refetch;

wire [4:0]  invtlb_op;
wire [9:0]  rj_asid;
wire [31:0] rk_va;

//EXE reg
reg        exe_valid;
reg [31:0] exe_pc;
reg        exe_rf_we;
reg [4:0 ] exe_dest;
reg        exe_sel_rf_res;
reg [31:0] exe_src1;
reg [31:0] exe_src2;
reg [18:0] exe_alu_op;
reg        exe_data_sram_en;
reg [31:0] exe_rkd_value;

reg [4:0]  exe_ld_what;
reg [2:0]  exe_st_what;

reg [1:0]  exe_rdcnt_what;
reg        exe_is_rdcntid;

reg [11:0] exe_csr_num;
reg [2:0]  exe_csr_what;
reg        exe_csr_we;
reg [31:0] exe_csr_wmask;

reg        exe_ex;
reg        exe_ertn_flush;
reg        exe_exc_syscall;
reg        exe_exc_adef;
reg        exe_exc_break;
reg        exe_exc_ine;
reg        exe_exc_ppi;
reg        exe_exc_tlbr;
reg        exe_exc_pif;
reg        exe_int;
reg [31:0] exe_badv;

reg        exe_is_invtlb;
reg        exe_is_tlbwr;
reg        exe_is_tlbfill;
reg        exe_is_tlbrd;
reg        exe_is_tlbsrch;
reg        exe_need_refetch;

reg [4:0]  exe_invtlb_op;
reg [9:0]  exe_rj_asid;
reg [31:0] exe_rk_va;

//MEM reg
reg        mem_valid;
reg [31:0] mem_pc;
reg        mem_rf_we;
reg [4:0]  mem_dest;
reg        mem_sel_rf_res;
reg [31:0] mem_alu_res;

reg [1:0]  mem_addr_low2; //to save the lowest 2 bits of read address.
reg [4:0]  mem_ld_what;
reg [2:0]  mem_st_what;

reg [1:0]  mem_rdcnt_what;
reg        mem_is_rdcntid;

reg [11:0] mem_csr_num;
reg [2:0]  mem_csr_what;
reg        mem_csr_we;
reg [31:0] mem_csr_wmask;

reg        mem_ex;
reg        mem_ertn_flush;
reg        mem_exc_syscall;
reg        mem_exc_adef;
reg        mem_exc_break;
reg        mem_exc_ine;
reg        mem_exc_ale;
reg        mem_exc_ppi_m;
reg        mem_exc_ppi_f;
reg        mem_exc_tlbr_m;
reg        mem_exc_tlbr_f;
reg        mem_exc_pif;
reg        mem_exc_pme;
reg        mem_exc_pis;
reg        mem_exc_pil;
reg        mem_exc_adem;
reg        mem_int;
reg [31:0] mem_badv;

reg        mem_is_invtlb;
reg        mem_is_tlbwr;
reg        mem_is_tlbfill;
reg        mem_is_tlbrd;
reg        mem_need_refetch;

//WB reg
reg        wb_valid;
reg [31:0] wb_pc;
reg        wb_rf_we;
reg [4:0]  wb_dest; 
reg [31:0] wb_res;

reg [1:0]  wb_rdcnt_what;
reg        wb_is_rdcntid;

reg [11:0] wb_csr_num;
reg [2:0]  wb_csr_what;
reg        wb_csr_we;
reg [31:0] wb_csr_wmask;

reg        wb_ex;
reg        wb_ertn_flush;
reg        wb_exc_syscall;
reg        wb_exc_adef;
reg        wb_exc_break;
reg        wb_exc_ine;
reg        wb_exc_ale;
reg        wb_exc_ppi_m;
reg        wb_exc_ppi_f;
reg        wb_exc_tlbr_m;
reg        wb_exc_tlbr_f;
reg        wb_exc_pif;
reg        wb_exc_pme;
reg        wb_exc_pis;
reg        wb_exc_pil;
reg        wb_exc_adem;
reg        wb_int;
reg [31:0] wb_badv; //bad address

reg        wb_is_tlbwr;
reg        wb_is_tlbfill;
reg        wb_is_tlbrd;
reg        wb_need_refetch;

reg [31:0] inst_sram_rdata_r;
reg        inst_buf_valid;

reg        if_br_or_ex_cancel;

reg [31:0] data_sram_rdata_buf;
reg        data_buf_valid;

reg        ex_and_data_addr_shaken;

reg        br_valid;
reg [31:0] br_buf;
reg        br_taken_buf;

wire pre_if_ready_go;
wire pre_if_to_if_valid;

wire if_allowin;
wire if_ready_go;
wire if_to_id_valid;

wire id_allowin;
wire id_ready_go;
wire id_to_exe_valid;

wire exe_allowin;
wire exe_ready_go;
wire exe_to_mem_valid;

wire mem_allowin;
wire mem_ready_go;
wire mem_to_wb_valid;

wire wb_allowin;
wire wb_ready_go;

wire [31:0] csr_rvalue;
wire        final_csr_we;
wire        has_int;

//tlb
wire [18:0] s0_vppn;
wire        s0_va_bit12;
wire [ 9:0] s0_asid;

wire [18:0] s1_vppn;
wire        s1_va_bit12;
wire [ 9:0] s1_asid;

wire        s0_found;
wire [ 3:0] s0_index;
wire [19:0] s0_ppn;
wire [ 5:0] s0_ps;
wire [ 1:0] s0_plv;
wire [ 1:0] s0_mat;
wire        s0_d;
wire        s0_v;

wire        s1_found;
wire [ 3:0] s1_index;
wire [19:0] s1_ppn;
wire [ 5:0] s1_ps;
wire [ 1:0] s1_plv;
wire [ 1:0] s1_mat;
wire        s1_d;
wire        s1_v;

wire [ 3:0] w_index;
wire [ 5:0] w_ps;
wire        w_ne;
wire [18:0] w_vppn;
wire [26:0] w_lo0_bus;
wire [26:0] w_lo1_bus;
wire [ 9:0] w_asid;

wire        tlb_we;
wire        tlb_w_g;
wire [19:0] tlb_w_ppn0;
wire [ 1:0] tlb_w_plv0;
wire [ 1:0] tlb_w_mat0;
wire        tlb_w_d0;
wire        tlb_w_v0;
wire [19:0] tlb_w_ppn1;
wire [ 1:0] tlb_w_plv1;
wire [ 1:0] tlb_w_mat1;
wire        tlb_w_d1;
wire        tlb_w_v1;
wire [ 5:0] r_ecode;

wire        r_e;
wire [18:0] r_vppn;
wire [ 5:0] r_ps;
wire [ 9:0] r_asid;
wire        r_g;
wire [19:0] r_ppn0;
wire [ 1:0] r_plv0;
wire [ 1:0] r_mat0;
wire        r_d0;
wire        r_v0;
wire [19:0] r_ppn1;
wire [ 1:0] r_plv1;
wire [ 1:0] r_mat1;
wire        r_d1;
wire        r_v1;
wire [26:0] tlb_lo0_bus;
wire [26:0] tlb_lo1_bus;
wire        tlb_ne_rd;
wire [ 5:0] tlb_ps;
wire [ 9:0] tlb_asid;
wire [18:0] tlb_vppn;

reg [63:0] timer;
always @(posedge aclk) begin
    if (reset) begin
        timer <= 64'b0;
    end
    else begin
        timer <= timer + 1;
    end
end

wire         inst_addr_shake;
wire         inst_data_shake;
wire         data_addr_shake;
wire         data_data_shake;
wire        pipe_reflush;
assign pipe_reflush = (wb_ex || wb_ertn_flush || wb_need_refetch) && wb_valid;

assign inst_addr_shake = inst_sram_req && inst_sram_addr_ok && !pipe_reflush;
assign inst_data_shake = inst_sram_data_ok;
assign data_addr_shake = data_sram_req && data_sram_addr_ok;
assign data_data_shake = data_sram_data_ok;
/****************************************************pre-IF***********************************************/
wire [ 1:0]   csr_crmd_plv;
wire          csr_crmd_da;
wire          csr_crmd_pg;
wire [31:0]   csr_dmw0;
wire [31:0]   csr_dmw1;
wire        if_will_be_mem;
wire        need_refetch;

//001 -> csr_dmw0    010 -> csr_dmw1   011 -> tlb   100 -> exception 
assign if_addr_mode = ((csr_crmd_da == 1'b1) && (csr_crmd_pg == 1'b0))                         ? 3'b000 : 
                     !((csr_crmd_da == 1'b0) && (csr_crmd_pg == 1'b1))                         ? 3'b100 :
                     ((csr_dmw0[0] || csr_dmw0[3]) && (nextpc[31:29] == csr_dmw0[31:29]) && !(csr_crmd_plv == 2'd3 && !csr_dmw0[3] || csr_crmd_plv == 2'd0 && !csr_dmw0[0])) ? 3'b001 : 
                     ((csr_dmw1[0] || csr_dmw1[3]) && (nextpc[31:29] == csr_dmw1[31:29]) && !(csr_crmd_plv == 2'd3 && !csr_dmw1[3] || csr_crmd_plv == 2'd0 && !csr_dmw1[0])) ? 3'b010 : 
                     3'b011;
assign if_tlb_addr  = (s1_ps == 6'd22) ? {s1_ppn[19:10], nextpc[21:0]} : {s1_ppn, nextpc[11:0]};

assign exc_adef     = !(if_addr_mode == 3'b001 || if_addr_mode == 3'b010) && 
                       ((nextpc[1:0] != 2'b00) || nextpc[31] && (csr_crmd_plv == 2'd3));
assign exc_tlbr_f   = (if_addr_mode == 3'b011) && !s1_found && !exc_adef;
assign exc_pif      = (if_addr_mode == 3'b011) && !s1_v && !exc_tlbr_f && !exc_adef;
assign exc_ppi_f    = (if_addr_mode == 3'b011) && (csr_crmd_plv == 3'b11) && (s1_plv == 3'b00) && !exc_pif && !exc_tlbr_f && !exc_adef;

wire pre_if_exc;
assign pre_if_exc = exc_adef || exc_pif || exc_tlbr_f || exc_ppi_f;

assign inst_sram_wstrb    = 4'b0;
assign inst_sram_wr       = |inst_sram_wstrb;
assign inst_sram_size     = 2'b10;
assign inst_sram_wdata    = 32'b0;
assign inst_sram_req      = aresetn && if_allowin && !br_stall && !pre_if_exc &&
                            (!(final_inst[31:26] == 6'h13) || !if_valid) && 
                            (!if_will_be_mem || !if_valid) && 
                            (((ld_what == 5'b0) && (st_what == 3'b0)) || !id_valid) && 
                            (!(|exe_ld_what || |exe_st_what) || !exe_valid) && 
                            (mem_allowin || !mem_valid || pipe_reflush);
assign inst_sram_addr = (if_addr_mode == 3'b000) ? nextpc :
                        (if_addr_mode == 3'b011) ? (s1_found ? if_tlb_addr : 32'b0)  :
                        (if_addr_mode == 3'b001) ? {csr_dmw0[27:25], nextpc[28:0]} : 
                        (if_addr_mode == 3'b010) ? {csr_dmw1[27:25], nextpc[28:0]} :
                        32'b0;

assign seq_pc       = pc + 3'h4;
assign nextpc       = ((wb_ex || wb_ertn_flush) && wb_valid) ? csr_rvalue :
                      (wb_need_refetch && wb_valid) ? wb_pc : 
                      (br_valid ? br_buf :
                      ((br_taken && !br_stall) ? br_target : seq_pc));

assign pre_if_ready_go      = inst_addr_shake || pre_if_exc;
assign pre_if_to_if_valid   = pre_if_ready_go;

always@(posedge aclk) begin
    if(reset) begin
        br_valid <= 1'b0;
    end
    else if(pre_if_to_if_valid && if_allowin) begin
        br_valid <= 1'b0;
    end
    else if(pipe_reflush) begin
        br_valid <= 1'b1;
        br_buf <= csr_rvalue;
    end
    else if(br_taken && !br_stall) begin
        br_valid <= 1'b1;
        br_buf <= br_target;
    end
end 

//br_buf is to temporarily store unusual nextpc. when br_taken or exception detected in wb, nextpc will be an abnormal value(not pc + 4). 
//when !if_allowin(last inst has not returned), if we don't store this abnormal address, threr's a great chance to lose it after this cycle.

/****************************************************pre-IF***********************************************/


/****************************************************IF begins***********************************************/
assign if_ready_go  = (inst_data_shake && !if_br_or_ex_cancel) || inst_buf_valid || if_ex;
assign if_allowin   = !if_valid || if_ready_go && id_allowin;
assign if_to_id_valid = if_valid && if_ready_go;

always @(posedge aclk) begin
    if (reset) begin
        if_valid <= 1'b0;
    end
    else if (if_allowin) begin
        if_valid <= pre_if_to_if_valid;
    end
    else if(pipe_reflush && (!pre_if_to_if_valid || pre_if_exc)) begin
        if_valid <= 1'b0;
    end
    else if (br_taken && !br_stall) begin
        if_valid <= 1'b0;
    end
    //when exception seen in WB, why don't we reset if_valid? is it still appropriate to do so?
    
    if (reset) begin
        pc <= 32'h1bff_fffc; 
    end
    else if (pre_if_to_if_valid && if_allowin) begin
        if_ex       <= pre_if_exc;
        if_exc_adef <= exc_adef;
        if_exc_ppi  <= exc_ppi_f;
        if_exc_tlbr <= exc_tlbr_f;
        if_exc_pif  <= exc_pif;
        if_badv     <= (pre_if_exc) ? nextpc : 32'b0;
        pc          <= nextpc;
    end
end
                      
assign if_will_be_mem = (final_inst[31:26] == 6'h0a) && ((final_inst[25:22] == 4'h0) ||
                                                              (final_inst[25:22] == 4'h1) || 
                                                              (final_inst[25:22] == 4'h2) || 
                                                              (final_inst[25:22] == 4'h4) || 
                                                              (final_inst[25:22] == 4'h5) || 
                                                              (final_inst[25:22] == 4'h6) || 
                                                              (final_inst[25:22] == 4'h8) || 
                                                              (final_inst[25:22] == 4'h9));
assign need_refetch = (inst_csrwr | inst_csrxchg | inst_tlbwr | inst_tlbfill | inst_invtlb | inst_tlbrd) && id_valid;

always@(posedge aclk) begin
    if(reset) begin
        inst_buf_valid <= 1'b0;
    end
    else begin
        if(pipe_reflush) begin
            inst_buf_valid <= 1'b0;
        end
        else if(if_ready_go && !id_allowin && !if_br_or_ex_cancel) begin
            inst_buf_valid <= 1'b1;
            inst_sram_rdata_r <= inst_sram_rdata;
        end
        else if(/*!if_ready_go && */id_allowin) begin 
            inst_buf_valid <= 1'b0;
        end
        else begin
            inst_buf_valid <= 1'b0;
        end
    end
end
//inst_sram_rdata is also volatile. but mention that if this inst should be cancelled, then it should also not go into our buffer.

always@(posedge aclk) begin
    if(reset) begin
        if_br_or_ex_cancel <= 1'b0;
    end
    else begin
        if(!if_allowin && !if_ready_go && ((br_taken && !br_stall) || pipe_reflush)) begin
            if_br_or_ex_cancel <= 1'b1;
        end
        else if(inst_data_shake) begin
            if_br_or_ex_cancel <= 1'b0;
        end
    end
end
//this signal is to tell IF that you should discard next return inst, and so if_ready_go should not be 1.

/****************************************************ID begins***********************************************/

//deal with data relevence
wire need_rkj;
wire need_rdj;
wire need_rj;
wire need_rd;
wire exe_rj_block;
wire exe_rk_block;
wire exe_rd_block;
wire mem_rj_block;
wire mem_rk_block;
wire mem_rd_block;
wire wb_rj_block;
wire wb_rk_block;
wire wb_rd_block;

wire [31:0] exe_pass;
wire [31:0] mem_pass;
wire [31:0] wb_pass;

assign need_rkj = inst_add_w | inst_sub_w | inst_slt | inst_sltu | inst_nor | inst_xor | inst_or | inst_and |
                  inst_sll_w | inst_srl_w | inst_sra_w | inst_mul_w | inst_mulh_w | inst_mulh_wu | inst_div_w | 
                  inst_mod_w | inst_div_wu | inst_mod_wu | inst_invtlb;
assign need_rdj = src_reg_is_rd;
assign need_rj = inst_slli_w | inst_srli_w | inst_srai_w | inst_addi_w | inst_jirl | inst_ld_w | inst_ld_b | inst_ld_bu | inst_ld_h | inst_ld_hu | 
                 inst_andi | inst_xori | inst_ori | inst_slti | inst_sltui;
assign need_rd = inst_csrwr | inst_csrxchg;

assign exe_rj_block = exe_valid && exe_rf_we && rj && exe_dest == rj && (need_rj || need_rkj || need_rdj);
assign exe_rk_block = exe_valid && exe_rf_we && rk && exe_dest == rk && need_rkj;
assign exe_rd_block = exe_valid && exe_rf_we && rd && exe_dest == rd && (need_rdj | need_rd);

assign mem_rj_block = mem_valid && mem_rf_we && rj && mem_dest == rj && (need_rj || need_rkj || need_rdj);
assign mem_rk_block = mem_valid && mem_rf_we && rk && mem_dest == rk && need_rkj;
assign mem_rd_block = mem_valid && mem_rf_we && rd && mem_dest == rd && (need_rdj | need_rd);

assign wb_rj_block = wb_valid && wb_rf_we && rj && wb_dest == rj && (need_rj || need_rkj || need_rdj);
assign wb_rk_block = wb_valid && wb_rf_we && rk && wb_dest == rk && need_rkj;
assign wb_rd_block = wb_valid && wb_rf_we && rd && wb_dest == rd && (need_rdj | need_rd);
               
//data pass
assign exe_pass = alu_result;
assign mem_pass = final_result;
assign wb_pass = rf_wdata;

assign rj_value  = (exe_rj_block) ? exe_pass :
                   (mem_rj_block) ? mem_pass :
                   (wb_rj_block) ? wb_pass : rf_rdata1;
                   
assign rkd_value = (exe_rk_block || exe_rd_block) ? exe_pass :
                   (mem_rk_block || mem_rd_block) ? mem_pass :
                   (wb_rk_block || wb_rd_block) ? wb_pass : rf_rdata2;

assign id_ready_go  = (pipe_reflush || ((exe_ex || exe_ertn_flush || exe_need_refetch) && exe_valid) || ((mem_ex || mem_ertn_flush || mem_need_refetch) && mem_valid)) ? 1'b1 : 
                       !(
                        (exe_sel_rf_res && (exe_rj_block || exe_rk_block || exe_rd_block)) || //block for ld
                        (mem_sel_rf_res && (mem_rj_block || mem_rk_block || mem_rd_block))                                  ||
                        ((|exe_csr_what | exe_is_rdcntid) && (exe_rj_block || exe_rk_block || exe_rd_block)) || //block for reading csrs's destination
                        ((|mem_csr_what | mem_is_rdcntid) && (mem_rj_block || mem_rk_block || mem_rd_block)) //block for reading csrs's destination
                        );
assign id_allowin   = !id_valid || id_ready_go && exe_allowin;
assign id_to_exe_valid = id_valid && id_ready_go;

//detect exceptions in ID
assign ertn_flush = inst_ertn;
assign exc_syscall = inst_syscall && !id_ex;
assign exc_break   = inst_break && !id_ex;
assign exc_ine     = !( (|alu_op) | inst_b | inst_beq | inst_bne | 
                                inst_blt | inst_bge | inst_bltu | inst_bgeu |
                                inst_csrrd |  inst_ertn | inst_syscall | inst_break |
                                inst_rdcntid | inst_rdcntvh_w | inst_rdcntvl_w |
                                inst_tlbfill | inst_tlbwr | inst_tlbrd | inst_tlbsrch | inst_invtlb
                    ) | (inst_invtlb & 
                          (invtlb_op[4] == 1'b1) | (invtlb_op[3] == 1'b1) | (invtlb_op [2:0] == 3'b111)) && !id_ex;

assign invtlb_op = inst_invtlb ? inst[4:0] : 5'b0;

assign final_inst =  inst_buf_valid ? inst_sram_rdata_r : inst_sram_rdata;
always @(posedge aclk) begin
    if (reset) begin
        id_valid <= 1'b0;
    end
    else if(pipe_reflush) begin
        id_valid <= 1'b0;
    end
    else if (br_taken && !br_stall && exe_allowin) begin
        id_valid <= 1'b0;
    end
    else if (id_allowin) begin
        id_valid <= if_to_id_valid;
    end
    
    if (if_to_id_valid && id_allowin) begin
        id_ex       <= if_ex;
        id_exc_adef <= if_exc_adef;
        id_exc_pif  <= if_exc_pif;
        id_exc_ppi  <= if_exc_ppi;
        id_exc_tlbr <= if_exc_tlbr;
        id_badv     <= if_badv;
        id_pc       <= pc;
        inst        <= final_inst;
        id_need_refetch <= need_refetch;
    end
end

//ld and st
assign ld_what = inst_ld_b  ? 5'b00001 : 
                 inst_ld_bu ? 5'b00010 : 
                 inst_ld_h  ? 5'b00100 : 
                 inst_ld_hu ? 5'b01000 : 
                 inst_ld_w  ? 5'b10000 : 5'b0;

assign st_what = inst_st_b  ? 3'b001 : 
                 inst_st_h  ? 3'b010 : 
                 inst_st_w  ? 3'b100 : 3'b0;

//rdcntvl and h
wire [1:0]  rdcnt_what;
assign rdcnt_what = (inst_rdcntvl_w) ? 2'b01 :
                   (inst_rdcntvh_w) ? 2'b10 :
                   2'b00;

//csr operations
wire [2:0] csr_what;
assign csr_what = inst_csrrd   ? 3'b001 : 
                  inst_csrwr   ? 3'b010 : 
                  inst_csrxchg ? 3'b100 : 
                  3'b0;

wire [11:0] csr_num;
assign csr_num = (exc_syscall || exc_break || exc_ine || id_ex || has_int) ? 12'h00c : 
                 inst_ertn    ? 12'h006 : 
                 inst_rdcntid ? 12'h040 :
                 inst[21:10];

wire        csr_we;
assign csr_we = inst_csrwr | inst_csrxchg;

wire [31:0] csr_wmask;
assign csr_wmask = inst_csrwr ? {32{1'b1}} : rj_value;

assign op_31_26  = inst[31:26];
assign op_25_22  = inst[25:22];
assign op_21_20  = inst[21:20];
assign op_19_15  = inst[19:15];

assign op_14_10  = inst[14:10];
 
assign rd   = inst[ 4: 0];
assign rj   = inst[ 9: 5];
assign rk   = inst[14:10];

assign i12  = inst[21:10];
assign i20  = inst[24: 5];
assign i16  = inst[25:10];
assign i26  = {inst[ 9: 0], inst[25:10]};

decoder_6_64 u_dec0(.in(op_31_26 ), .out(op_31_26_d ));
decoder_4_16 u_dec1(.in(op_25_22 ), .out(op_25_22_d ));
decoder_2_4  u_dec2(.in(op_21_20 ), .out(op_21_20_d ));
decoder_5_32 u_dec3(.in(op_19_15 ), .out(op_19_15_d ));

assign inst_add_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h00];
assign inst_sub_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h02];
assign inst_slt    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h04];
assign inst_sltu   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h05];
assign inst_nor    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h08];
assign inst_and    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h09];
assign inst_or     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0a];
assign inst_xor    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0b];
assign inst_slli_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h01];
assign inst_srli_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h09];
assign inst_srai_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h11];
assign inst_addi_w = op_31_26_d[6'h00] & op_25_22_d[4'ha];
assign inst_ld_w   = op_31_26_d[6'h0a] & op_25_22_d[4'h2];
assign inst_st_w   = op_31_26_d[6'h0a] & op_25_22_d[4'h6];
assign inst_jirl   = op_31_26_d[6'h13];
assign inst_b      = op_31_26_d[6'h14];
assign inst_bl     = op_31_26_d[6'h15];
assign inst_beq    = op_31_26_d[6'h16];
assign inst_bne    = op_31_26_d[6'h17];
assign inst_lu12i_w= op_31_26_d[6'h05] & ~inst[25];

//exp10
assign inst_slti   = op_31_26_d[6'h00] & op_25_22_d[4'h8];
assign inst_sltui  = op_31_26_d[6'h00] & op_25_22_d[4'h9];
assign inst_andi   = op_31_26_d[6'h00] & op_25_22_d[4'hd];
assign inst_ori    = op_31_26_d[6'h00] & op_25_22_d[4'he];
assign inst_xori   = op_31_26_d[6'h00] & op_25_22_d[4'hf];
assign inst_sll_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0e];
assign inst_srl_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0f];
assign inst_sra_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h10];
assign inst_mul_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h18];
assign inst_mulh_w = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h19];
assign inst_mulh_wu= op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h1a];
assign inst_div_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h00];
assign inst_mod_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h01];
assign inst_div_wu = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h02];
assign inst_mod_wu = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h03];
assign inst_pcaddu12i = op_31_26_d[6'h07] & ~inst[25];
//exp10

//exp11
assign inst_blt    = op_31_26_d[6'h18];
assign inst_bge    = op_31_26_d[6'h19];
assign inst_bltu   = op_31_26_d[6'h1a];
assign inst_bgeu   = op_31_26_d[6'h1b];
assign inst_ld_b   = op_31_26_d[6'h0a] & op_25_22_d[4'h0];
assign inst_ld_h   = op_31_26_d[6'h0a] & op_25_22_d[4'h1];
assign inst_ld_bu  = op_31_26_d[6'h0a] & op_25_22_d[4'h8];
assign inst_ld_hu  = op_31_26_d[6'h0a] & op_25_22_d[4'h9];
assign inst_st_b   = op_31_26_d[6'h0a] & op_25_22_d[4'h4];
assign inst_st_h   = op_31_26_d[6'h0a] & op_25_22_d[4'h5];
//exp11

//exp12 & exp13
assign inst_csrrd    = op_31_26_d[6'h01] & (inst[25:24] == 2'b0) & (rj == 5'b0);
assign inst_csrwr    = op_31_26_d[6'h01] & (inst[25:24] == 2'b0) & (rj == 5'b1);
assign inst_csrxchg  = op_31_26_d[6'h01] & (inst[25:24] == 2'b0) & ~((rj == 5'b0) || (rj == 5'b1));
assign inst_ertn     = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & (rk == 5'b01110) & ({rj, rd} == 10'b0);
assign inst_syscall  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h16];
assign inst_break    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h14];

assign inst_rdcntvl_w= op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h0] & (rk == 5'b11000) & (rj == 5'b0);
assign inst_rdcntvh_w= op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h0] & (rk == 5'b11001) & (rj == 5'b0);
assign inst_rdcntid  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h0] & (rk == 5'b11000) & (rd == 5'b0);
//exp12 & exp13

//exp18
assign inst_invtlb  = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h13];
assign inst_tlbsrch = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & (rk == 5'b01010) & ({rj, rd} == 10'b0);
assign inst_tlbrd   = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & (rk == 5'b01011) & ({rj, rd} == 10'b0);
assign inst_tlbwr   = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & (rk == 5'b01100) & ({rj, rd} == 10'b0);
assign inst_tlbfill = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & (rk == 5'b01101) & ({rj, rd} == 10'b0);
//exp18

assign alu_op[ 0] = inst_add_w | inst_addi_w | inst_ld_b | inst_ld_bu | inst_ld_h | 
                    inst_ld_hu | inst_ld_w | inst_st_b | inst_st_h | inst_st_w |
                    inst_jirl | inst_bl | inst_pcaddu12i;
assign alu_op[ 1] = inst_sub_w;
assign alu_op[ 2] = inst_slt | inst_slti;
assign alu_op[ 3] = inst_sltu | inst_sltui;
assign alu_op[ 4] = inst_and | inst_andi;
assign alu_op[ 5] = inst_nor;
assign alu_op[ 6] = inst_or | inst_ori;
assign alu_op[ 7] = inst_xor | inst_xori;
assign alu_op[ 8] = inst_slli_w | inst_sll_w;
assign alu_op[ 9] = inst_srli_w | inst_srl_w;
assign alu_op[10] = inst_srai_w | inst_sra_w;
assign alu_op[11] = inst_lu12i_w | inst_csrwr | inst_csrxchg;
assign alu_op[12] = inst_mul_w;
assign alu_op[13] = inst_mulh_w;
assign alu_op[14] = inst_mulh_wu;
assign alu_op[15] = inst_div_wu;
assign alu_op[16] = inst_mod_wu;
assign alu_op[17] = inst_div_w;
assign alu_op[18] = inst_mod_w;

assign need_ui5   =  inst_slli_w | inst_srli_w | inst_srai_w;
assign need_si12  =  inst_addi_w | inst_ld_b | inst_ld_bu | inst_ld_h | inst_ld_hu | inst_ld_w  |
                     inst_st_b | inst_st_h | inst_st_w | inst_slti | inst_sltui;
assign need_ui12  =  inst_andi | inst_ori | inst_xori;
assign need_si16  =  inst_jirl | inst_beq | inst_bne | inst_blt | inst_bge | inst_bltu | inst_bgeu;
assign need_si20  =  inst_lu12i_w | inst_pcaddu12i;
assign need_si26  =  inst_b | inst_bl;
assign src2_is_4  =  inst_jirl | inst_bl;

assign imm = src2_is_4 ? 32'h4                      :
             need_si20 ? {i20[19:0], 12'b0}         :
             need_ui12 ? {{20{1'b0}}, i12[11:0]}    :
/*need_ui5 || need_si12*/{{20{i12[11]}}, i12[11:0]} ;

assign br_offs = need_si26 ? {{ 4{i26[25]}}, i26[25:0], 2'b0} :
                /*need_si16*/{{14{i16[15]}}, i16[15:0], 2'b0} ;

assign jirl_offs = {{14{i16[15]}}, i16[15:0], 2'b0};

assign src_reg_is_rd = inst_beq | inst_bne | inst_blt | inst_bge | inst_bltu | inst_bgeu | inst_st_b | 
                        inst_st_h | inst_st_w | inst_csrwr | inst_csrxchg;

assign src1_is_pc    = inst_jirl | inst_bl | inst_pcaddu12i;

assign src2_is_imm   = inst_slli_w |
                       inst_srli_w |
                       inst_srai_w |
                       inst_addi_w |
                       inst_ld_b   |
                       inst_ld_bu  |
                       inst_ld_h   |
                       inst_ld_hu  |
                       inst_ld_w   |
                       inst_st_b   |
                       inst_st_h   |
                       inst_st_w   |
                       inst_lu12i_w|
                       inst_jirl   |
                       inst_bl     |
                       inst_slti   |
                       inst_sltui  |
                       inst_andi   |
                       inst_ori    |
                       inst_xori   |
                       inst_pcaddu12i;

assign res_from_mem  = inst_ld_b | inst_ld_bu | inst_ld_h | inst_ld_hu | inst_ld_w;
assign mem_we        = inst_st_w | inst_st_h | inst_st_b;
assign dst_is_r1     = inst_bl;

//rdcntid
wire is_rdcntid;
assign is_rdcntid = inst_rdcntid;

//invtlb
assign rj_asid = !inst_invtlb ?  10'b0 :
                 (invtlb_op == 5'd4 || invtlb_op == 5'd5 || invtlb_op == 5'd6) ?  rj_value[9:0] : 10'b0;
assign rk_va   = !inst_invtlb ? 32'b0 :
                 (invtlb_op == 5'd5 || invtlb_op == 5'd6) ? rkd_value : 32'b0;

assign gr_we         = ~inst_st_b & ~inst_st_h & ~inst_st_w & ~inst_beq & ~inst_bne & ~inst_blt & ~inst_bge & 
                       ~inst_bltu & ~inst_bgeu & ~inst_b & ~inst_syscall & ~inst_ertn & ~inst_break & ~exc_ine & ~id_exc_adef & 
                       ~inst_tlbrd & ~inst_tlbwr & ~inst_tlbfill & ~inst_tlbsrch & ~inst_invtlb;

assign dest          = dst_is_r1  ? 5'd1 : 
                       is_rdcntid ? rj   :
                                    rd   ;

assign rf_raddr1 = rj;
assign rf_raddr2 = src_reg_is_rd ? rd : rk;
regfile u_regfile(
    .clk    (aclk     ),
    .raddr1 (rf_raddr1),
    .rdata1 (rf_rdata1),
    .raddr2 (rf_raddr2),
    .rdata2 (rf_rdata2),
    .we     (rf_we    ),
    .waddr  (rf_waddr ),
    .wdata  (rf_wdata )
    );

assign rj_eq_rd   = (rj_value == rkd_value);
assign rj_lt_rd   = $signed(rj_value) < $signed(rkd_value);
assign rj_lt_rd_u = rj_value < rkd_value;
assign br_taken = (   inst_beq  &&  rj_eq_rd
                   || inst_bne  && !rj_eq_rd
                   || inst_blt  &&  rj_lt_rd
                   || inst_bge  && !rj_lt_rd
                   || inst_bltu &&  rj_lt_rd_u
                   || inst_bgeu && !rj_lt_rd_u
                   || inst_jirl || inst_bl || inst_b
                  ) && id_valid && id_ready_go;
                  
assign br_target = (inst_beq || inst_bne || inst_blt || inst_bge || inst_bltu || inst_bgeu || inst_bl || inst_b) ? (id_pc + br_offs) :
                                                   /*inst_jirl*/ (rj_value + jirl_offs);   

assign br_stall  = !(pipe_reflush || ((exe_ex || exe_ertn_flush || exe_need_refetch) && exe_valid) || ((mem_ex || mem_ertn_flush || mem_need_refetch) && mem_valid)) && 
                    (exe_sel_rf_res && (exe_rj_block || exe_rk_block || exe_rd_block)) && 
                    (inst_beq || inst_bne || inst_blt || inst_bltu || inst_bge || inst_bgeu || inst_b || inst_bl || inst_jirl);
                                                   
assign alu_src1 = src1_is_pc  ? id_pc[31:0] : ((|csr_what) ? 32'b0 : rj_value);
assign alu_src2 = src2_is_imm ? imm : rkd_value;

/****************************************************EXE begins***********************************************/
wire [3:0]  st_b_we;
wire [3:0]  st_h_we;
wire [3:0]  st_w_we;
wire [3:0]  final_we;
wire [11:0] exe_add_csr_num;
wire        exe_exc;
wire        need_mem;
assign need_mem = (|exe_ld_what) || (|exe_st_what);

//001 -> csr_dmw0    010 -> csr_dmw1   011 -> tlb   100 -> exception 
assign mem_addr_mode =((csr_crmd_da == 1'b1) && (csr_crmd_pg == 1'b0))                         ? 3'b000 : 
                     !((csr_crmd_da == 1'b0) && (csr_crmd_pg == 1'b1))                         ? 3'b100 :
                     ((csr_dmw0[0] || csr_dmw0[3]) && (alu_result[31:29] == csr_dmw0[31:29]) && !(csr_crmd_plv == 2'd3 && !csr_dmw0[3] || csr_crmd_plv == 2'd0 && !csr_dmw0[0])) ? 3'b001 : 
                     ((csr_dmw1[0] || csr_dmw1[3]) && (alu_result[31:29] == csr_dmw1[31:29]) && !(csr_crmd_plv == 2'd3 && !csr_dmw1[3] || csr_crmd_plv == 2'd0 && !csr_dmw1[0])) ? 3'b010 : 
                     3'b011;
assign mem_tlb_addr = (s0_ps == 6'd22) ? {s0_ppn[19:10], alu_result[21:0]} : {s0_ppn, alu_result[11:0]};

assign exe_exc   = exc_ppi_m || exc_tlbr_m || exc_adem || exc_ale || exc_pis || exc_pil || exc_pme;
assign exc_ale   = (
                        (exe_ld_what == 5'b00100 /*ld_h*/|| 
                        exe_ld_what == 5'b01000 /*ld_hu*/|| 
                        exe_st_what == 3'b010/*st_h*/) && alu_result[0] != 1'b0
                    ) || (
                        (exe_ld_what == 5'b10000 /*ld_w*/|| 
                        exe_st_what == 3'b100/*st_w*/) && alu_result[1:0] != 2'b0
                    ) && !exe_ex;
assign exc_adem  = need_mem && !(mem_addr_mode == 3'b001 || mem_addr_mode == 3'b010) && 
                    alu_result[31] && (csr_crmd_plv == 2'd3) && !exc_ale && !exe_ex;
assign exc_tlbr_m = need_mem && (mem_addr_mode == 3'b011) && !s0_found && !exc_ale && !exe_ex && !exc_adem;
assign exc_pis   = (|exe_st_what) && (mem_addr_mode == 3'b011) && !s0_v && !exc_tlbr_m && !exe_ex && !exc_adem && !exc_ale;
assign exc_pil   = (|exe_ld_what) && (mem_addr_mode == 3'b011) && !s0_v && !exc_tlbr_m && !exe_ex && !exc_adem && !exc_ale;
assign exc_ppi_m = need_mem && (mem_addr_mode == 3'b011) && (csr_crmd_plv == 3'b011) && (s0_plv == 3'b000) && 
                    !exc_pis && !exc_pil && !exc_tlbr_m && !exe_ex && !exc_adem && !exc_ale;
assign exc_pme   = (|exe_st_what) && (mem_addr_mode == 3'b011) && !s0_d && !exc_pis && !exc_pil && !exc_tlbr_m &&
                    !exc_ppi_m && !exe_ex && !exc_adem && !exc_ale;

assign exe_ready_go  = (!need_mem || data_addr_shake || exe_exc) && (
                                ((exe_ex || exe_need_refetch || exe_ertn_flush) && exe_valid) || 
                                ((mem_ex || mem_need_refetch || mem_ertn_flush) && mem_valid) || 
                                pipe_reflush || (alu_ready && !tlb_block)
                            );
assign exe_allowin   = !exe_valid || exe_ready_go && mem_allowin;
assign exe_to_mem_valid = exe_valid && exe_ready_go;

always @(posedge aclk) begin
    if (reset) begin
        exe_valid <= 1'b0;
        exe_data_sram_en <= 1'b0;
    end
    else if(pipe_reflush) begin
        exe_valid <= 1'b0;
    end
    else if (exe_allowin) begin
        exe_valid <= id_to_exe_valid;
    end
    
    if (id_to_exe_valid && exe_allowin) begin
        exe_pc         <= id_pc;
        exe_rf_we      <= gr_we;
        exe_dest       <= dest;
        exe_sel_rf_res <= res_from_mem;
        exe_src1       <= alu_src1;
        exe_src2       <= alu_src2;
        exe_alu_op     <= alu_op;
        exe_data_sram_en <= (res_from_mem || mem_we) && id_valid;
        exe_rkd_value    <= rkd_value;
        exe_ld_what      <= ld_what;
        exe_st_what      <= st_what;

        exe_rdcnt_what   <= rdcnt_what;
        exe_is_rdcntid   <= is_rdcntid; 

        exe_csr_num      <= csr_num;
        exe_csr_what     <= csr_what;
        exe_csr_we       <= csr_we; 
        exe_csr_wmask    <= csr_wmask;

        exe_ex                <= id_ex || has_int || exc_syscall || exc_break || exc_ine;
        exe_int               <= has_int;
        exe_exc_syscall       <= exc_syscall;
        exe_exc_break         <= exc_break;
        exe_exc_ine           <= exc_ine;
        exe_exc_adef          <= id_exc_adef;
        exe_ertn_flush        <= ertn_flush;
        exe_exc_pif           <= id_exc_pif;
        exe_exc_ppi           <= id_exc_ppi;
        exe_exc_tlbr          <= id_exc_tlbr;
        exe_badv              <= id_badv;

        exe_is_invtlb         <= inst_invtlb;
        exe_is_tlbwr          <= inst_tlbwr;
        exe_is_tlbfill        <= inst_tlbfill;
        exe_need_refetch      <= id_need_refetch;
        exe_is_tlbsrch        <= inst_tlbsrch;
        exe_is_tlbrd          <= inst_tlbrd;

        exe_invtlb_op         <= invtlb_op;
        exe_rj_asid           <= rj_asid;
        exe_rk_va             <= rk_va;
    end
end

alu u_alu(
    .alu_op     (exe_alu_op    ),
    .alu_src1   (exe_src1      ),
    .alu_src2   (exe_src2      ),
    .alu_result (alu_result    ),
    .alu_ready  (alu_ready     ),
    .clk        (aclk          ),
    .reset      (reset         ),
    .wb_ex      (wb_ex && wb_valid),
    .wb_ertn_flush(wb_ertn_flush && wb_valid),
    .wb_need_refetch(wb_need_refetch && wb_valid)
    );

assign st_b_we = (alu_result[1:0] == 2'b00) ? 4'b0001 : 
                 (alu_result[1:0] == 2'b01) ? 4'b0010 : 
                 (alu_result[1:0] == 2'b10) ? 4'b0100 :
                                              4'b1000 ;
                                              
assign st_h_we = (alu_result[1:0] == 2'b00) ? 4'b0011 : 
                                              4'b1100 ;
assign st_w_we = 4'b1111;

assign final_we = exe_st_what[0]    ? st_b_we    : 
                  exe_st_what[1]    ? st_h_we    :
                  exe_st_what[2]    ? st_w_we    : 
                  4'b0;

assign data_sram_req   = aresetn && exe_data_sram_en && exe_valid && !exe_exc && mem_allowin && 
                        !((mem_ex || mem_ertn_flush || mem_need_refetch) && mem_valid) && !pipe_reflush;
assign data_sram_wstrb = (!exe_exc && exe_valid && !pipe_reflush && 
                            !((exe_ex || exe_need_refetch) && exe_valid) && !((mem_ex || mem_ertn_flush || mem_need_refetch) && mem_valid)
                         ) ? final_we : 4'b0;
assign data_sram_wr    = |data_sram_wstrb;
assign data_sram_addr = (mem_addr_mode == 3'b000) ? alu_result :
                        (mem_addr_mode == 3'b011) ? (s0_found ? mem_tlb_addr : 32'b0 )      :
                        (mem_addr_mode == 3'b001) ? {csr_dmw0[27:25], alu_result[28:0]} : 
                        (mem_addr_mode == 3'b010) ? {csr_dmw1[27:25], alu_result[28:0]} :
                        32'b0;
assign data_sram_wdata = exe_st_what[0]    ? {4{exe_rkd_value[7:0]}}  : 
                         exe_st_what[1]    ? {2{exe_rkd_value[15:0]}} :
                                             exe_rkd_value;
assign data_sram_size  = (exe_ld_what[4] || exe_st_what[2]) ? 2'b10 : 
                         (exe_ld_what[2] || exe_ld_what[3] || exe_st_what[1]) ? 2'b01 : 
                         2'b0;

wire [31:0] exe_wire_badv;
assign exe_wire_badv = (exe_badv != 32'b0) ? exe_badv :
                       (exe_exc) ? alu_result : 32'b0;
assign exe_add_csr_num = (exe_exc) ? 12'hc : exe_csr_num;
/****************************************************MEM begins***********************************************/
assign mem_ready_go  = (data_buf_valid || 
                        ~(|mem_ld_what) && ~(|mem_st_what)) || ((data_data_shake && !ex_and_data_addr_shaken)
                       ) || mem_ex;
assign mem_allowin   = !mem_valid || mem_ready_go && wb_allowin;
assign mem_to_wb_valid = mem_valid && mem_ready_go;

always @(posedge aclk) begin
    if (reset) begin
        mem_valid <= 1'b0;
    end
    else if(pipe_reflush) begin
        mem_valid <= 1'b0;
    end
    else if (mem_allowin) begin
        mem_valid <= exe_to_mem_valid;
    end
    
    if (exe_to_mem_valid && mem_allowin) begin
        mem_ld_what    <= exe_ld_what;
        mem_st_what    <= exe_st_what;
        mem_pc         <= exe_pc;
        mem_rf_we      <= exe_rf_we;
        mem_dest       <= exe_dest;
        mem_sel_rf_res <= exe_sel_rf_res;
        mem_alu_res    <= alu_result;
        mem_addr_low2  <= alu_result[1:0];
        mem_ld_what    <= exe_ld_what;
        mem_csr_num    <= exe_add_csr_num;
        mem_csr_what   <= exe_csr_what;
        mem_csr_we     <= exe_csr_we;
        mem_csr_wmask  <= exe_csr_wmask;

        mem_rdcnt_what <= exe_rdcnt_what;
        mem_is_rdcntid <= exe_is_rdcntid;

        mem_ex                <= exe_ex || exc_adem || exc_ale || exc_ppi_m || exc_tlbr_m || exc_pis || exc_pil || exc_pme;
        mem_int               <= exe_int;
        mem_exc_syscall       <= exe_exc_syscall;
        mem_exc_break         <= exe_exc_break;
        mem_exc_ine           <= exe_exc_ine;
        mem_exc_adef          <= exe_exc_adef;
        mem_ertn_flush        <= exe_ertn_flush;
        mem_exc_ale           <= exc_ale;
        mem_exc_adem          <= exc_adem;
        mem_exc_ppi_f         <= exe_exc_ppi;
        mem_exc_tlbr_f        <= exe_exc_tlbr;
        mem_exc_ppi_m         <= exc_ppi_m;
        mem_exc_tlbr_m        <= exc_tlbr_m;
        mem_exc_pif           <= exe_exc_pif;
        mem_exc_pil           <= exc_pil;
        mem_exc_pis           <= exc_pis;
        mem_exc_pme           <= exc_pme;
        mem_badv              <= exe_wire_badv;

        mem_is_invtlb         <= exe_is_invtlb;
        mem_is_tlbwr          <= exe_is_tlbwr;
        mem_is_tlbfill        <= exe_is_tlbfill;
        mem_need_refetch      <= exe_need_refetch;
        mem_is_tlbrd          <= exe_is_tlbrd;
    end
end

wire [31:0] ld_b_res;
wire [31:0] ld_bu_res;
wire [31:0] ld_h_res;
wire [31:0] ld_hu_res;
wire [31:0] ld_w_res;

wire [31:0] final_data_rdata;

assign ld_b_res  = (mem_addr_low2 == 2'b00) ? {{24{final_data_rdata[7]}}, final_data_rdata[7:0]} : 
                   (mem_addr_low2 == 2'b01) ? {{24{final_data_rdata[15]}}, final_data_rdata[15:8]} : 
                   (mem_addr_low2 == 2'b10) ? {{24{final_data_rdata[23]}}, final_data_rdata[23:16]} : 
                                              {{24{final_data_rdata[31]}}, final_data_rdata[31:24]};
                                              
assign ld_bu_res = (mem_addr_low2 == 2'b00) ? {24'b0, final_data_rdata[7:0]} : 
                   (mem_addr_low2 == 2'b01) ? {24'b0, final_data_rdata[15:8]} : 
                   (mem_addr_low2 == 2'b10) ? {24'b0, final_data_rdata[23:16]} : 
                                              {24'b0, final_data_rdata[31:24]};
                                              
assign ld_h_res  = (mem_addr_low2 == 2'b00) ? {{16{final_data_rdata[15]}}, final_data_rdata[15:0]} : 
                                              {{16{final_data_rdata[31]}}, final_data_rdata[31:16]};
                                              
assign ld_hu_res = (mem_addr_low2 == 2'b00) ? {16'b0, final_data_rdata[15:0]} : 
                                              {16'b0, final_data_rdata[31:16]};
                                              
assign ld_w_res  = final_data_rdata;

assign mem_result   = mem_ld_what[0] ? ld_b_res : 
                      mem_ld_what[1] ? ld_bu_res : 
                      mem_ld_what[2] ? ld_h_res : 
                      mem_ld_what[3] ? ld_hu_res : 
                      ld_w_res;

wire [31:0] final_mem_result;
assign final_data_rdata = data_buf_valid ? data_sram_rdata_buf : data_sram_rdata;

assign final_result = mem_sel_rf_res ? mem_result : mem_alu_res;

always@(posedge aclk) begin
    if(reset) begin
        data_buf_valid <= 1'b0;
    end
    else begin
        if(pipe_reflush) begin
            data_buf_valid <= 1'b0;
        end
        else if(mem_valid && mem_ready_go && !wb_allowin) begin
            data_buf_valid <= 1'b1;
            data_sram_rdata_buf <= data_sram_rdata;
        end
        else if(wb_allowin) begin 
            data_buf_valid <= 1'b0;
        end
        else begin
            data_buf_valid <= 1'b0;
        end
    end
end

always@(posedge aclk) begin
    if(reset) begin
        ex_and_data_addr_shaken <= 1'b0;
    end
    else begin
        if(!mem_allowin && !mem_ready_go && pipe_reflush) begin
            ex_and_data_addr_shaken <= 1'b1;
        end
        else if(data_data_shake) begin
            ex_and_data_addr_shaken <= 1'b0;
        end
    end
end

assign tlb_block = mem_valid && (mem_is_tlbrd || (|mem_csr_what)) && exe_valid && exe_is_tlbsrch;

/****************************************************WB begins***********************************************/
wire rf_we_allow;
assign rf_we_allow  = 1'b1;
assign wb_ready_go  = 1'b1;
assign wb_allowin   = !wb_valid || wb_ready_go && rf_we_allow;

always @(posedge aclk) begin
    if (reset) begin
        wb_valid <= 1'b0;
    end
    else if(pipe_reflush) begin
        wb_valid <= 1'b0;
    end
    else if (wb_allowin) begin
        wb_valid <= mem_to_wb_valid;
    end
    
    if (mem_to_wb_valid && wb_allowin) begin
        wb_pc          <= mem_pc;
        wb_rf_we       <= mem_rf_we;
        wb_dest        <= mem_dest;
        wb_res         <= final_result;
        wb_csr_num     <= mem_csr_num;
        wb_csr_what    <= mem_csr_what;
        wb_csr_we      <= mem_csr_we;
        wb_csr_wmask   <= mem_csr_wmask;

        wb_rdcnt_what  <= mem_rdcnt_what;
        wb_is_rdcntid  <= mem_is_rdcntid;

        wb_ex                  <= mem_ex;
        wb_int                 <= mem_int;
        wb_exc_syscall         <= mem_exc_syscall;
        wb_exc_break           <= mem_exc_break;
        wb_exc_ine             <= mem_exc_ine;
        wb_exc_adef            <= mem_exc_adef;
        wb_exc_ale             <= mem_exc_ale;
        wb_ertn_flush          <= mem_ertn_flush;
        wb_exc_tlbr_m          <= mem_exc_tlbr_m;
        wb_exc_tlbr_f          <= mem_exc_tlbr_f;
        wb_exc_adem            <= mem_exc_adem;
        wb_exc_pil             <= mem_exc_pil;
        wb_exc_pis             <= mem_exc_pis;
        wb_exc_pif             <= mem_exc_pif;
        wb_exc_pme             <= mem_exc_pme;
        wb_exc_ppi_m           <= mem_exc_ppi_m;
        wb_exc_ppi_f           <= mem_exc_ppi_f;
        wb_badv                <= mem_badv;

        wb_is_tlbwr            <= mem_is_tlbwr;
        wb_is_tlbfill          <= mem_is_tlbfill;
        wb_need_refetch        <= mem_need_refetch;
        wb_is_tlbrd            <= mem_is_tlbrd;
    end
end
assign rf_we    = (wb_valid && !wb_ex && !wb_ertn_flush && !wb_need_refetch) ? wb_rf_we : 1'b0;
assign rf_waddr = wb_dest;
assign rf_wdata = (|wb_csr_what || wb_is_rdcntid) ? csr_rvalue   :
                  (wb_rdcnt_what[0])              ? timer[31:0]  :
                  (wb_rdcnt_what[1])              ? timer[63:32] :
                                                    wb_res;

assign final_csr_we = (wb_valid & !wb_ex) ? wb_csr_we : 1'b0;

wire [ 5:0] wb_ecode;
wire [ 8:0] wb_esubcode;
wire [ 7:0] hw_int_in; 
wire        ipi_int_in;
wire [31:0] coreid_in;

assign wb_ecode = (wb_int         ) ? 6'h0 :
                  (wb_exc_adef    ) ? 6'h8 :
                  (wb_exc_tlbr_f  ) ? 6'h3f :
                  (wb_exc_pif     ) ? 6'h3 :
                  (wb_exc_ppi_f   ) ? 6'h7 :
                  (wb_exc_break   ) ? 6'hc :
                  (wb_exc_syscall ) ? 6'hb :
                  (wb_exc_ine     ) ? 6'hd :
                  (wb_exc_ale     ) ? 6'h9 : 
                  (wb_exc_adem    ) ? 6'h8 : 
                  (wb_exc_tlbr_m  ) ? 6'h3f :
                  (wb_exc_pil     ) ? 6'h1 :
                  (wb_exc_pis     ) ? 6'h2 :
                  (wb_exc_ppi_m   ) ? 6'h7 :
                  (wb_exc_pme     ) ? 6'h4 : 6'h0;
assign wb_esubcode = (wb_exc_adem) ? 9'b1 : 9'b0;
assign hw_int_in   = 8'b0;
assign ipi_int_in  = 1'b0;
assign coreid_in   = 32'b0;

wire        final_w_e;
assign final_w_e = wb_is_tlbwr ? ~w_ne : ((r_ecode == 6'h3f) ? 1'b1 : ~w_ne);

wire [ 3:0] final_w_index;
assign final_w_index = wb_is_tlbwr ? w_index : {need_si12, final_inst[5], final_result[6], csr_rvalue[6]};

wire [11:0] final_csr_num;
assign final_csr_num = (wb_ecode == 6'h3f) ? 12'h088 : wb_csr_num;

assign tlb_we = (wb_is_tlbwr | wb_is_tlbfill) && wb_valid;
assign tlb_w_v0 = w_lo0_bus[0];
assign tlb_w_d0 = w_lo0_bus[1];
assign tlb_w_plv0 = w_lo0_bus[3:2];
assign tlb_w_mat0 = w_lo0_bus[5:4];
assign tlb_w_g = w_lo0_bus[6];
assign tlb_w_ppn0 = w_lo0_bus[26:7];
assign tlb_w_v1 = w_lo1_bus[0];
assign tlb_w_d1 = w_lo1_bus[1];
assign tlb_w_plv1 = w_lo1_bus[3:2];
assign tlb_w_mat1 = w_lo1_bus[5:4];
assign tlb_w_ppn1 = w_lo1_bus[26:7];

assign tlb_ne_rd    = ~r_e;
assign tlb_lo0_bus  = tlb_ne_rd ? 27'b0 : {r_ppn0, r_g, r_mat0, r_plv0, r_d0, r_v0};//{PPN, G, MAT, PLV, D, V}
assign tlb_lo1_bus  = tlb_ne_rd ? 27'b0 : {r_ppn1, r_g, r_mat1, r_plv1, r_d1, r_v1};//{PPN, G, MAT, PLV, D, V}
assign tlb_ps       = tlb_ne_rd ?  6'b0 : s1_ps; 
assign tlb_asid     = tlb_ne_rd ? 10'b0 : r_asid;
assign tlb_vppn     = tlb_ne_rd ? 19'b0 : r_vppn;

csr u_csr(
    .clk        (aclk        ),
    .reset      (reset       ),
    .csr_wvalue (wb_res      ),
    .csr_wmask  (wb_csr_wmask),
    .csr_we     (final_csr_we),
    .csr_num    (final_csr_num),
    .csr_rvalue (csr_rvalue  ),
    .wb_ex      (wb_ex && wb_valid),
    .ertn_flush (wb_ertn_flush && wb_valid),
    .wb_ecode   (wb_ecode    ),
    .wb_esubcode(wb_esubcode ),
    .wb_pc      (wb_pc       ),
    .has_int    (has_int     ),
    .hw_int_in  (hw_int_in   ),
    .ipi_int_in (ipi_int_in  ),
    .wb_vaddr   (wb_badv     ),
    .coreid_in  (coreid_in   ),

    .is_tlbsrch (exe_is_tlbsrch && exe_valid),
    .is_tlbrd   (wb_is_tlbrd && wb_valid),
    .tlbsrch_hits (s0_found ),
    .tlb_index  (s0_index   ),
    .tlb_ps     (tlb_ps     ),
    .tlb_ne_rd  (tlb_ne_rd  ),
    .tlb_vppn   (tlb_vppn   ),
    .tlb_lo0_bus(tlb_lo0_bus),
    .tlb_lo1_bus(tlb_lo1_bus),
    .tlb_asid   (tlb_asid   ),
    .tlb_index_r(w_index    ),
    .tlb_ps_r   (w_ps       ),
    .tlb_ne_r   (w_ne       ),
    .tlb_vppn_r (w_vppn     ),
    .tlb_lo0_bus_r(w_lo0_bus),
    .tlb_lo1_bus_r(w_lo1_bus),
    .tlb_asid_r (w_asid     ),
    .ecode      (r_ecode    ),

    .crmd_plv_r (csr_crmd_plv),
    .crmd_da_r  (csr_crmd_da),
    .crmd_pg_r  (csr_crmd_pg),
    .dmw0_r     (csr_dmw0),
    .dmw1_r     (csr_dmw1)
);

assign s0_va_bit12 = exe_is_tlbsrch ? 1'b0 : alu_result[12];
assign s0_asid = exe_is_invtlb ? exe_rj_asid : w_asid;
assign s0_vppn = exe_is_tlbsrch ? w_vppn           : 
                 exe_is_invtlb  ? exe_rk_va[31:13] :
                 alu_result[31:13];

assign s1_va_bit12 = nextpc[12];
assign s1_asid = w_asid;
assign s1_vppn = nextpc[31:13];

wire ex;
assign ex = (id_valid && (id_ex || has_int || exc_syscall || exc_break || exc_ine || ertn_flush || id_need_refetch)) || 
            (exe_valid && (exe_ex || exe_ertn_flush || exe_need_refetch)) || 
            (mem_valid && (mem_ex || mem_ertn_flush || mem_need_refetch));
bridge u_bridge(
    .clk                (aclk),
    .reset              (reset),
    .ex                 (ex),

    .inst_sram_req      (inst_sram_req),
    .inst_sram_wr       (inst_sram_wr),
    .inst_sram_size     (inst_sram_size),
    .inst_sram_addr     (inst_sram_addr),
    .inst_sram_wdata    (inst_sram_wdata),
    .inst_sram_wstrb    (inst_sram_wstrb),
    .inst_sram_addr_ok  (inst_sram_addr_ok),
    .inst_sram_data_ok  (inst_sram_data_ok),
    .inst_sram_rdata    (inst_sram_rdata),

    .data_sram_req      (data_sram_req),
    .data_sram_wr       (data_sram_wr),
    .data_sram_size     (data_sram_size),
    .data_sram_addr     (data_sram_addr),
    .data_sram_wstrb    (data_sram_wstrb),
    .data_sram_wdata    (data_sram_wdata),
    .data_sram_addr_ok  (data_sram_addr_ok),
    .data_sram_data_ok  (data_sram_data_ok),
    .data_sram_rdata    (data_sram_rdata),

    .arid               (arid),
    .araddr             (araddr),
    .arlen              (arlen),
    .arsize             (arsize),
    .arburst            (arburst),
    .arlock             (arlock),
    .arcache            (arcache),
    .arprot             (arprot),
    .arvalid            (arvalid),
    .arready            (arready),

    .rready             (rready),
    .rid                (rid),
    .rdata              (rdata),
    .rresp              (rresp),
    .rlast              (rlast),
    .rvalid             (rvalid),

    .awid               (awid),
    .awaddr             (awaddr),
    .awlen              (awlen),
    .awsize             (awsize),
    .awburst            (awburst),
    .awlock             (awlock),
    .awcache            (awcache),
    .awprot             (awprot),
    .awvalid            (awvalid),
    .awready            (awready),

    .wid                (wid),
    .wdata              (wdata),
    .wstrb              (wstrb),
    .wlast              (wlast),
    .wvalid             (wvalid),
    .wready             (wready),

    .bready             (bready),
    .bid                (bid),
    .bresp              (bresp),
    .bvalid             (bvalid)
);

tlb u_tlb(
    .clk                (aclk),

    .s0_vppn            (s0_vppn),
    .s0_va_bit12        (s0_va_bit12),
    .s0_asid            (s0_asid),
    .s0_found           (s0_found),
    .s0_index           (s0_index),
    .s0_ppn             (s0_ppn),
    .s0_ps              (s0_ps),
    .s0_plv             (s0_plv),
    .s0_mat             (s0_mat),
    .s0_d               (s0_d),
    .s0_v               (s0_v),

    .s1_vppn            (s1_vppn),
    .s1_va_bit12        (s1_va_bit12),
    .s1_asid            (s1_asid),
    .s1_found           (s1_found),
    .s1_index           (s1_index),
    .s1_ppn             (s1_ppn),
    .s1_ps              (s1_ps),
    .s1_plv             (s1_plv),
    .s1_mat             (s1_mat),
    .s1_d               (s1_d),
    .s1_v               (s1_v),

    .invtlb_valid       (exe_is_invtlb),
    .invtlb_op          (exe_invtlb_op),

    .we                 (tlb_we),
    .w_index            (final_w_index),
    .w_e                (final_w_e),
    .w_vppn             (w_vppn),
    .w_ps               (w_ps),
    .w_asid             (w_asid),
    .w_g                (tlb_w_g),
    .w_ppn0             (tlb_w_ppn0),
    .w_plv0             (tlb_w_plv0),
    .w_mat0             (tlb_w_mat0),
    .w_d0               (tlb_w_d0),
    .w_v0               (tlb_w_v0),
    .w_ppn1             (tlb_w_ppn1),
    .w_plv1             (tlb_w_plv1),
    .w_mat1             (tlb_w_mat1),
    .w_d1               (tlb_w_d1),
    .w_v1               (tlb_w_v1),

    .r_index            (w_index),
    .r_e                (r_e),
    .r_vppn             (r_vppn),
    .r_ps               (r_ps),
    .r_asid             (r_asid),
    .r_g                (r_g),
    .r_ppn0             (r_ppn0),
    .r_plv0             (r_plv0),
    .r_mat0             (r_mat0),
    .r_d0               (r_d0),
    .r_v0               (r_v0),
    .r_ppn1             (r_ppn1),
    .r_plv1             (r_plv1),
    .r_mat1             (r_mat1),
    .r_d1               (r_d1),
    .r_v1               (r_v1)
);

// debug info generate
assign debug_wb_pc       = wb_pc;
assign debug_wb_rf_we    = {4{rf_we}};
assign debug_wb_rf_wnum  = wb_dest;
assign debug_wb_rf_wdata = rf_wdata;

endmodule