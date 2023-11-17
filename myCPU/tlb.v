module tlb
#(
    parameter TLBNUM = 16
)//该模块支持取指和访存两个部分的虚实地�??转换序求
(
    //search port 0 (for fetch) 0用于TLBSRCH指令查找操作
    input  wire                      clk,
    input  wire [18:0]               s0_vppn,
    input  wire                      s0_va_bit12,//？第12位有�?么用�?
    input  wire [9:0]                s0_asid,
    output wire                      s0_found,
    output wire [$clog2(TLBNUM)-1:0] s0_index,//记录命中在第几项，信息填入到CSR.TLBIDX?
    //注意$clog2，即取后面操作数�?2为底的对数，这里index的位数为4
    output wire [19:0]               s0_ppn,
    output wire [5:0]                s0_ps,
    output wire [1:0]                s0_plv,
    output wire [1:0]                s0_mat,
    output wire                      s0_d,
    output wire                      s0_v,

    //search port 1 (for load/store) 1用于访存指令查找TLB
    input  wire [18:0]               s1_vppn,
    input  wire                      s1_va_bit12,
    input  wire [9:0]                s1_asid,
    output wire                      s1_found,
    output wire [$clog2(TLBNUM)-1:0] s1_index,
    output wire [19:0]               s1_ppn,
    output wire [5:0]                s1_ps,
    output wire [1:0]                s1_plv,
    output wire [1:0]                s1_mat,
    output wire                      s1_d,
    output wire                      s1_v,

    //invtlb opcode   TLB模块要支持INVTLB指令的查找无效操作，不同op对应的查找新信息可以复用访存查找tlb的端�??
    //invtlb_op用于表示invtlb的具体操作类�??
    input  wire                      invtlb_valid,
    input  wire [4:0]                invtlb_op,
    
    //write port  TLB要支持TLBWR和TLBFILL指令的写操作
    input  wire                      we,//写使�?
    input  wire [$clog2(TLBNUM)-1:0] w_index,
    input  wire                      w_e,
    input  wire [18:0]               w_vppn,
    input  wire [5:0]                w_ps,
    input  wire [9:0]                w_asid,
    input  wire                      w_g,
    input  wire [19:0]               w_ppn0,
    input  wire [1:0]                w_plv0,
    input  wire [1:0]                w_mat0,
    input  wire                      w_d0,
    input  wire                      w_v0,
    input  wire [19:0]               w_ppn1,
    input  wire [1:0]                w_plv1,
    input  wire [1:0]                w_mat1,
    input  wire                      w_d1,
    input  wire                      w_v1,

    //read port  TLB要支持TLBRD指令的读操作
    input  wire [$clog2(TLBNUM)-1:0] r_index,
    output wire                      r_e,
    output wire [18:0]               r_vppn,
    output wire [5:0]                r_ps,
    output wire [9:0]                r_asid,
    output wire                      r_g,
    output wire [19:0]               r_ppn0,
    output wire [1:0]                r_plv0,
    output wire [1:0]                r_mat0,
    output wire                      r_d0,
    output wire                      r_v0,
    output wire [19:0]               r_ppn1,
    output wire [1:0]                r_plv1,
    output wire [1:0]                r_mat1,
    output wire                      r_d1,
    output wire                      r_v1
);

reg [TLBNUM-1:0] tlb_e;
reg [TLBNUM-1:0] tlb_ps4MB;//pagesize 1:4MB 0:4KB
reg [18:0]       tlb_vppn   [TLBNUM-1:0];
reg [9:0]        tlb_asid   [TLBNUM-1:0];
reg              tlb_g      [TLBNUM-1:0];
reg [19:0]       tlb_ppn0   [TLBNUM-1:0];
reg [1:0]        tlb_plv0   [TLBNUM-1:0];
reg [1:0]        tlb_mat0   [TLBNUM-1:0];
reg              tlb_d0     [TLBNUM-1:0];
reg              tlb_v0     [TLBNUM-1:0];
reg [19:0]       tlb_ppn1   [TLBNUM-1:0];
reg [1:0]        tlb_plv1   [TLBNUM-1:0];
reg [1:0]        tlb_mat1   [TLBNUM-1:0];
reg              tlb_d1     [TLBNUM-1:0];
reg              tlb_v1     [TLBNUM-1:0];

wire s0_parity;//用于判断选取双页中的哪一�??奇数页还是偶数页�?
wire s1_parity;

//read
assign r_e    = tlb_e[r_index];
assign r_vppn = tlb_vppn[r_index];
assign r_ps   = tlb_ps4MB[r_index] ? 6'd22 : 6'd12;
//指令手册5.4.2，loongarch只支�?4MB�?4KB页，ps�?22对应4MB�?12对应4KB
assign r_asid = tlb_asid[r_index];
assign r_g    = tlb_g[r_index];
assign r_ppn0 = tlb_ppn0[r_index];
assign r_plv0 = tlb_plv0[r_index];
assign r_mat0 = tlb_mat0[r_index];
assign r_d0   = tlb_d0[r_index];
assign r_v0   = tlb_v0[r_index];
assign r_ppn1 = tlb_ppn1[r_index];
assign r_plv1 = tlb_plv1[r_index];
assign r_mat1 = tlb_mat1[r_index];
assign r_d1   = tlb_d1[r_index];
assign r_v1   = tlb_v1[r_index];

//write

always @(posedge clk) begin
  if(we == 1'b1) begin
    tlb_vppn[w_index] <= w_vppn;
    tlb_ps4MB[w_index]<= (w_ps == 6'd22) ? 1'b1 : 1'b0;
    tlb_asid[w_index] <= w_asid;
    tlb_g[w_index]    <= w_g;
    tlb_ppn0[w_index] <= w_ppn0;
    tlb_plv0[w_index] <= w_plv0;
    tlb_mat0[w_index] <= w_mat0;
    tlb_d0[w_index]   <= w_d0;
    tlb_v0[w_index]   <= w_v0;
    tlb_ppn1[w_index] <= w_ppn1;
    tlb_plv1[w_index] <= w_plv1;
    tlb_mat1[w_index] <= w_mat1;
    tlb_d1[w_index]   <= w_d1;
    tlb_v1[w_index]   <= w_v1;
  end
end

//search
reg [15:0] match0;
reg [15:0] match1;
//search 0 for fetch

integer i;
always @(*) begin
    for(i = 0; i < 16; i = i + 1)begin
        match0[i] = tlb_e[i]
                 && (s0_vppn[18:10]==tlb_vppn[i][18:10])
                 && (tlb_ps4MB[i] || s0_vppn[9:0]==tlb_vppn[i][9:0])
                 && ((s0_asid==tlb_asid[i]) || tlb_g[i]);

        match1[i] = tlb_e[i]
                 && (s1_vppn[18:10]==tlb_vppn[i][18:10])
                 && (tlb_ps4MB[i] || s1_vppn[9:0]==tlb_vppn[i][9:0])
                 && ((s1_asid==tlb_asid[i]) || tlb_g[i]);
    end
end

assign s0_found = |match0;//查找命中的found就是看match是否不等于全0
//如果match为全0，则查找失败，found�?0

//命中项的PFN等信息读出操作�?�辑可以参�??3.1节select信号是译码后位向量信息的多路选择�??
//这里采用第二种方法以避免不必要的优先级关�?
assign s0_index = ({4{match0[0]}}  & 4'b0000)
                | ({4{match0[1]}}  & 4'b0001)
                | ({4{match0[2]}}  & 4'b0010)
                | ({4{match0[3]}}  & 4'b0011)
                | ({4{match0[4]}}  & 4'b0100)
                | ({4{match0[5]}}  & 4'b0101)
                | ({4{match0[6]}}  & 4'b0110)
                | ({4{match0[7]}}  & 4'b0111)
                | ({4{match0[8]}}  & 4'b1000)
                | ({4{match0[9]}}  & 4'b1001)
                | ({4{match0[10]}} & 4'b1010)
                | ({4{match0[11]}} & 4'b1011)
                | ({4{match0[12]}} & 4'b1100)
                | ({4{match0[13]}} & 4'b1101)
                | ({4{match0[14]}} & 4'b1110)
                | ({4{match0[15]}} & 4'b1111);

assign s0_ps  = tlb_ps4MB[s0_index] ? 6'd22 : 6'd12;//22对应4MB,12对应4KB
//assign a0_parity = s0_vppn[9];//这个�?4M页拆成两�?2M页对应的奇偶？还要根据s0_va_bit12分析4K页的奇偶
//4M页拆成两�?2M页存在TLB中是�?么含义？TLB内只能存�?2M页？
assign s0_parity = tlb_ps4MB[s0_index] ? s0_vppn[9] : s0_va_bit12;

assign s0_v   = s0_parity ? tlb_v1[s0_index]   : tlb_v0[s0_index];
assign s0_d   = s0_parity ? tlb_d1[s0_index]   : tlb_d0[s0_index];
assign s0_mat = s0_parity ? tlb_mat1[s0_index] : tlb_mat0[s0_index];
assign s0_plv = s0_parity ? tlb_plv1[s0_index] : tlb_plv0[s0_index];
assign s0_ppn = s0_parity ? tlb_ppn1[s0_index] : tlb_ppn0[s0_index];

//search 1 for load/store
assign s1_found = |match1;
assign s1_index = ({4{match1[0]}}  & 4'b0000)
                | ({4{match1[1]}}  & 4'b0001)
                | ({4{match1[2]}}  & 4'b0010)
                | ({4{match1[3]}}  & 4'b0011)
                | ({4{match1[4]}}  & 4'b0100)
                | ({4{match1[5]}}  & 4'b0101)
                | ({4{match1[6]}}  & 4'b0110)
                | ({4{match1[7]}}  & 4'b0111)
                | ({4{match1[8]}}  & 4'b1000)
                | ({4{match1[9]}}  & 4'b1001)
                | ({4{match1[10]}} & 4'b1010)
                | ({4{match1[11]}} & 4'b1011)
                | ({4{match1[12]}} & 4'b1100)
                | ({4{match1[13]}} & 4'b1101)
                | ({4{match1[14]}} & 4'b1110)
                | ({4{match1[15]}} & 4'b1111);

assign s1_ps  = tlb_ps4MB[s1_index] ? 6'd22 : 6'd12;//22对应4MB,12对应4KB
//assign s1_parity = s1_vppn[9];
assign s1_parity = tlb_ps4MB[s1_index] ? s1_vppn[9] : s1_va_bit12;

assign s1_v   = s1_parity ? tlb_v1[s1_index]   : tlb_v0[s1_index];
assign s1_d   = s1_parity ? tlb_d1[s1_index]   : tlb_d0[s1_index];
assign s1_mat = s1_parity ? tlb_mat1[s1_index] : tlb_mat0[s1_index];
assign s1_plv = s1_parity ? tlb_plv1[s1_index] : tlb_plv0[s1_index];
assign s1_ppn = s1_parity ? tlb_ppn1[s1_index] : tlb_ppn0[s1_index];

//INVTLB指令相关
/*
  input  wire                      invtlb_valid,
  input  wire [4:0]                invtlb_op,
*/
reg [15:0] inv_match_G;
reg [15:0] inv_match_ASID;
reg [15:0] inv_match_VA;

//invtlb_op=0/1时清空所有页表项
integer k;
always @(*) begin
    for(k = 0; k < 16; k = k + 1)begin
        inv_match_G[k] = tlb_g[k];
        inv_match_ASID[k] = (s0_asid == tlb_asid[k] || tlb_g[k]);
        inv_match_VA[k] = (s0_vppn[18:10] == tlb_vppn[k][18:10])             
                       && (tlb_ps4MB[k] || s0_vppn[9:0] == tlb_vppn[k][9:0]);
    end
end

/*
assign inv_match_G[0]    = tlb_g[0];
assign inv_match_ASID[0] = (s1_asid == tlb_asid[0]) || tlb_g[0];
assign inv_match_VA[0]   = (s1_vppn[18:10] == tlb_vppn[0][18:10])             
                        && (tlb_ps4MB[0] || s1_vppn[9:0] == tlb_vppn[15][9:0]);
*/

integer j;
always @(posedge clk) begin
    if(we == 1'b1) begin
        tlb_e[w_index]    <= w_e;
    end
    else if(invtlb_valid) begin
        for(j = 0; j < 16; j = j + 1)begin
            if((invtlb_op == 5'd0) || (invtlb_op == 5'd1)) begin
                tlb_e[j] <= 1'b0;
            end
            else if((invtlb_op == 5'd2) && inv_match_G[j]) begin
                tlb_e[j] <= 1'b0;
            end
            else if((invtlb_op == 5'd3) && !inv_match_G[j]) begin
                tlb_e[j] <= 1'b0;
            end
            else if((invtlb_op == 5'd4) && !inv_match_G[j] && inv_match_ASID[j]) begin
                tlb_e[j] <= 1'b0;
            end
            else if((invtlb_op == 5'd5) && !inv_match_G[j] && inv_match_ASID[j] && inv_match_VA[j]) begin
                tlb_e[j] <= 1'b0;
            end 
            else if((invtlb_op == 5'd6) && match0[j]) begin
                tlb_e[j] <= 1'b0;
            end
            else begin
                tlb_e[j] <= tlb_e[j];
            end
        end
    end
end


endmodule