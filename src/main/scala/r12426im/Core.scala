package r12426im
// * 增加流水线支持

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._

class Core extends Module {
  val io = IO(new Bundle {
    val imem = Flipped(new ImemPortIo())
    val dmem = Flipped(new DmemPortIo())
    val exit = Output(Bool())
    val gp = Output(UInt(WORD_LEN.W))
  })

  val regfile = Mem(32, UInt(WORD_LEN.W))
  val csr_regfile = Mem(4096, UInt(WORD_LEN.W))

  // ********* Pipeline Registers *********
  // IF/ID state
  val id_reg_pc = RegInit(0.U(WORD_LEN.W))
  val id_reg_inst = RegInit(0.U(WORD_LEN.W))

  // ID/EX state
  val exe_reg_pc = RegInit(0.U(WORD_LEN.W))
  val exe_reg_wb_addr = RegInit(0.U(ADDR_LEN.W))
  val exe_reg_op1_data = RegInit(0.U(WORD_LEN.W))
  val exe_reg_op2_data = RegInit(0.U(WORD_LEN.W))
  val exe_reg_rs2_data = RegInit(0.U(WORD_LEN.W))
  val exe_reg_exe_fun = RegInit(0.U(EXE_FUN_LEN.W))
  val exe_reg_mem_wen = RegInit(0.U(MEN_LEN.W))
  val exe_reg_rf_wen = RegInit(0.U(REN_LEN.W))
  val exe_reg_wb_sel = RegInit(0.U(WB_SEL_LEN.W))
  val exe_reg_csr_addr = RegInit(0.U(CSR_ADDR_LEN.W))
  val exe_reg_csr_cmd = RegInit(0.U(CSR_LEN.W))
  val exe_reg_imm_i_sext = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_s_sext = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_b_sext = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_u_shifted = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_z_uext = RegInit(0.U(WORD_LEN.W))

  // EX/MEM state
  val mem_reg_pc = RegInit(0.U(WORD_LEN.W))
  val mem_reg_wb_addr = RegInit(0.U(ADDR_LEN.W))
  val mem_reg_op1_data = RegInit(0.U(WORD_LEN.W))
  val mem_reg_rs2_data = RegInit(0.U(WORD_LEN.W))
  val mem_reg_mem_wen = RegInit(0.U(MEN_LEN.W))
  val mem_reg_rf_wen = RegInit(0.U(REN_LEN.W))
  val mem_reg_wb_sel = RegInit(0.U(WB_SEL_LEN.W))
  val mem_reg_csr_addr = RegInit(0.U(CSR_ADDR_LEN.W))
  val mem_reg_csr_cmd = RegInit(0.U(CSR_LEN.W))
  val mem_reg_imm_z_uext = RegInit(0.U(WORD_LEN.W))
  val mem_reg_alu_out = RegInit(0.U(WORD_LEN.W))

  // MEM/WB state
  val wb_reg_wb_addr = RegInit(0.U(ADDR_LEN.W))
  val wb_reg_rf_wen = RegInit(0.U(REN_LEN.W))
  val wb_reg_wb_data = RegInit(0.U(WORD_LEN.W))

  // ********* Instruction Fetch (IF) Stage *********
  val if_reg_pc = RegInit(START_ADDR)
  io.imem.addr := if_reg_pc
  val if_inst = io.imem.inst

  val stall_flg = Wire(Bool())
  val exe_br_flg = Wire(Bool())
  val exe_br_target = Wire(UInt(WORD_LEN.W))
  val exe_jmp_flg = Wire(Bool())
  val exe_alu_out = Wire(UInt(WORD_LEN.W))

  val if_pc_plus4 = if_reg_pc + 4.U(WORD_LEN.W)
  val if_pc_next = MuxCase(
    if_pc_plus4,
    Seq(
      exe_br_flg -> exe_br_target,
      exe_jmp_flg -> exe_alu_out,
      (if_inst === ECALL) -> csr_regfile(0x305),
      stall_flg -> if_reg_pc // stall
    )
  )
  if_reg_pc := if_pc_next

  // ********* IF/ID Stage *********
  id_reg_pc := Mux(stall_flg, id_reg_pc, if_reg_pc)
  id_reg_inst := MuxCase(
    if_inst,
    Seq(
      (exe_br_flg || exe_jmp_flg) -> BUBBLE,
      stall_flg -> id_reg_inst
    )
  )

  // ********* Decode (ID) Stage *********
  val id_rs1_addr_b = id_reg_inst(19, 15)
  val id_rs2_addr_b = id_reg_inst(24, 20)

  // 与EX数据冒险 -> stall
  val id_rs1_data_hazard =
    (exe_reg_rf_wen === REN_S) && (id_rs1_addr_b =/= 0.U) && (id_rs1_addr_b === exe_reg_wb_addr)
  val id_rs2_data_hazard =
    (exe_reg_rf_wen === REN_S) && (id_rs2_addr_b =/= 0.U) && (id_rs2_addr_b === exe_reg_wb_addr)
  stall_flg := (id_rs1_data_hazard || id_rs2_data_hazard)

  val id_inst =
    Mux((exe_br_flg || exe_jmp_flg || stall_flg), BUBBLE, id_reg_inst)

  val id_rs1_addr = id_inst(19, 15)
  val id_rs2_addr = id_inst(24, 20)
  val id_wb_addr = id_inst(11, 7)

  val mem_wb_data = Wire(UInt(WORD_LEN.W))
  val id_rs1_data = MuxCase(
    regfile(id_rs1_addr),
    Seq(
      (id_rs1_addr === 0.U) -> 0.U(WORD_LEN.W),
      ((id_rs1_addr === mem_reg_wb_addr) && (mem_reg_rf_wen === REN_S)) -> mem_wb_data, // 从MEN直通
      ((id_rs1_addr === wb_reg_wb_addr) && (wb_reg_rf_wen === REN_S)) -> wb_reg_wb_data // 从WB直通
    )
  )
  val id_rs2_data = MuxCase(
    regfile(id_rs2_addr),
    Seq(
      (id_rs2_addr === 0.U) -> 0.U(WORD_LEN.W),
      ((id_rs2_addr === mem_reg_wb_addr) && (mem_reg_rf_wen === REN_S)) -> mem_wb_data, // 从MEN直通
      ((id_rs2_addr === wb_reg_wb_addr) && (wb_reg_rf_wen === REN_S)) -> wb_reg_wb_data // 从WB直通
    )
  )

  val id_imm_i = id_inst(31, 20)
  val id_imm_i_sext = Cat(Fill(20, id_imm_i(11)), id_imm_i)
  val id_imm_s = Cat(id_inst(31, 25), id_inst(11, 7))
  val id_imm_s_sext = Cat(Fill(20, id_imm_s(11)), id_imm_s)
  val id_imm_b = Cat(
    id_inst(31),
    id_inst(7),
    id_inst(30, 25),
    id_inst(11, 8)
  )
  val id_imm_b_sext = Cat(Fill(19, id_imm_b(11)), id_imm_b, 0.U)
  val id_imm_j = Cat(
    id_inst(31),
    id_inst(19, 12),
    id_inst(20),
    id_inst(30, 21)
  )
  val id_imm_j_sext = Cat(Fill(11, id_imm_j(19)), id_imm_j, 0.U)
  val id_imm_u = id_inst(31, 12)
  val id_imm_u_shifted = Cat(id_imm_u, Fill(12, 0.U))
  val id_imm_z = id_inst(19, 15)
  val id_imm_z_uext = Cat(Fill(27, 0.U), id_imm_z)

  val csignals = ListLookup(
    id_inst,
    List(ALU_X, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
    Array(
      LW -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_MEM, CSR_X),
      SW -> List(ALU_ADD, OP1_RS1, OP2_IMS, MEN_S, REN_X, WB_X, CSR_X),
      ADD -> List(ALU_ADD, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      ADDI -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SUB -> List(ALU_SUB, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      AND -> List(ALU_AND, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      OR -> List(ALU_OR, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      XOR -> List(ALU_XOR, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      ANDI -> List(ALU_AND, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      ORI -> List(ALU_OR, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      XORI -> List(ALU_XOR, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SLL -> List(ALU_SLL, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      SRL -> List(ALU_SRL, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      SRA -> List(ALU_SRA, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      SLLI -> List(ALU_SLL, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SRLI -> List(ALU_SRL, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SRAI -> List(ALU_SRA, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SLT -> List(ALU_SLT, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      SLTU -> List(ALU_SLTU, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      SLTI -> List(ALU_SLT, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SLTIU -> List(ALU_SLTU, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      BEQ -> List(BR_BEQ, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
      BNE -> List(BR_BNE, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
      BGE -> List(BR_BLT, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
      BGEU -> List(BR_BGE, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
      BLT -> List(BR_BLTU, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
      BLTU -> List(BR_BGEU, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
      JAL -> List(ALU_ADD, OP1_PC, OP2_IMJ, MEN_X, REN_S, WB_PC, CSR_X),
      JALR -> List(ALU_JALR, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_PC, CSR_X),
      LUI -> List(ALU_ADD, OP1_X, OP2_IMU, MEN_X, REN_S, WB_ALU, CSR_X),
      AUIPC -> List(ALU_ADD, OP1_PC, OP2_IMU, MEN_X, REN_S, WB_ALU, CSR_X),
      CSRRW -> List(ALU_COPY1, OP1_RS1, OP2_X, MEN_X, REN_S, WB_CSR, CSR_W),
      CSRRWI -> List(ALU_COPY1, OP1_IMZ, OP2_X, MEN_X, REN_S, WB_CSR, CSR_W),
      CSRRS -> List(ALU_COPY1, OP1_RS1, OP2_X, MEN_X, REN_S, WB_CSR, CSR_S),
      CSRRSI -> List(ALU_COPY1, OP1_IMZ, OP2_X, MEN_X, REN_S, WB_CSR, CSR_S),
      CSRRC -> List(ALU_COPY1, OP1_RS1, OP2_X, MEN_X, REN_S, WB_CSR, CSR_C),
      CSRRCI -> List(ALU_COPY1, OP1_IMZ, OP2_X, MEN_X, REN_S, WB_CSR, CSR_C),
      ECALL -> List(ALU_X, OP1_X, OP2_X, MEN_X, REN_X, WB_X, CSR_E),
      MUL -> List(ALU_MUL, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      DIV -> List(ALU_DIV, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      MULH -> List(ALU_MULH, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      MULHSU -> List(ALU_MULHSU, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      MULHU -> List(ALU_MULHU, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      DIVU -> List(ALU_DIVU, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      REM -> List(ALU_REM, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      REMU -> List(ALU_REMU, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X)
    )
  )

  val id_exe_fun :: id_op1_sel :: id_op2_sel :: id_mem_wen :: id_rf_wen :: id_wb_sel :: id_csr_cmd :: Nil =
    csignals

  val id_op1_data = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (id_op1_sel === OP1_RS1) -> id_rs1_data,
      (id_op1_sel === OP1_PC) -> id_reg_pc,
      (id_op1_sel === OP1_IMZ) -> id_imm_z_uext
    )
  )

  val id_op2_data = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (id_op2_sel === OP2_RS2) -> id_rs2_data,
      (id_op2_sel === OP2_IMI) -> id_imm_i_sext,
      (id_op2_sel === OP2_IMS) -> id_imm_s_sext,
      (id_op2_sel === OP2_IMJ) -> id_imm_j_sext,
      (id_op2_sel === OP2_IMU) -> id_imm_u_shifted
    )
  )
  val id_csr_addr =
    Mux(id_csr_cmd === CSR_E, 0x342.U(CSR_ADDR_LEN.W), id_inst(31, 20))

  // ********* Decode/Execute (ID/EX) Stage *********
  exe_reg_pc := id_reg_pc
  exe_reg_op1_data := id_op1_data
  exe_reg_op2_data := id_op2_data
  exe_reg_rs2_data := id_rs2_data
  exe_reg_wb_addr := id_wb_addr
  exe_reg_wb_sel := id_wb_sel
  exe_reg_mem_wen := id_mem_wen
  exe_reg_rf_wen := id_rf_wen
  exe_reg_csr_cmd := id_csr_cmd
  exe_reg_csr_addr := id_csr_addr
  exe_reg_imm_i_sext := id_imm_i_sext
  exe_reg_imm_s_sext := id_imm_s_sext
  exe_reg_imm_b_sext := id_imm_b_sext
  exe_reg_imm_u_shifted := id_imm_u_shifted
  exe_reg_imm_z_uext := id_imm_z_uext
  exe_reg_exe_fun := id_exe_fun

  // ********* Execute (EX) Stage *********
  exe_alu_out := MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (exe_reg_exe_fun === ALU_ADD) -> (exe_reg_op1_data + exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_SUB) -> (exe_reg_op1_data - exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_AND) -> (exe_reg_op1_data & exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_OR) -> (exe_reg_op1_data | exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_XOR) -> (exe_reg_op1_data ^ exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_SLL) -> (exe_reg_op1_data << exe_reg_op2_data(
        4,
        0
      ))(31, 0),
      (exe_reg_exe_fun === ALU_SRL) -> (exe_reg_op1_data >> exe_reg_op2_data(
        4,
        0
      )).asUInt,
      (exe_reg_exe_fun === ALU_SRA) -> (exe_reg_op1_data.asSInt >> exe_reg_op2_data(
        4,
        0
      )).asUInt,
      (exe_reg_exe_fun === ALU_SLT) -> (exe_reg_op1_data.asSInt < exe_reg_op2_data.asSInt).asUInt,
      (exe_reg_exe_fun === ALU_SLTU) -> (exe_reg_op1_data < exe_reg_op2_data).asUInt,
      (exe_reg_exe_fun === ALU_JALR) -> ((exe_reg_op1_data + exe_reg_op2_data) & (~1
        .U(WORD_LEN.W))),
      (exe_reg_exe_fun === ALU_COPY1) -> exe_reg_op1_data,
      (exe_reg_exe_fun === ALU_MUL) -> (exe_reg_op1_data * exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_DIV) -> (exe_reg_op1_data / exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_MULH) -> (exe_reg_op1_data.asSInt * exe_reg_op2_data.asSInt)(
        WORD_LEN * 2 - 1,
        WORD_LEN
      ),
      (exe_reg_exe_fun === ALU_MULHSU) -> (exe_reg_op1_data.asSInt * exe_reg_op2_data.asUInt)(
        WORD_LEN * 2 - 1,
        WORD_LEN
      ),
      (exe_reg_exe_fun === ALU_MULHU) -> (exe_reg_op1_data.asUInt * exe_reg_op2_data.asUInt)(
        WORD_LEN * 2 - 1,
        WORD_LEN
      ),
      (exe_reg_exe_fun === ALU_DIVU) -> (exe_reg_op1_data.asUInt / exe_reg_op2_data.asUInt),
      (exe_reg_exe_fun === ALU_REM) -> (exe_reg_op1_data % exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_REMU) -> (exe_reg_op1_data.asUInt % exe_reg_op2_data.asUInt)
    )
  )

  exe_br_flg := MuxCase(
    false.B,
    Seq(
      (exe_reg_exe_fun === BR_BEQ) -> (exe_reg_op1_data === exe_reg_op2_data),
      (exe_reg_exe_fun === BR_BNE) -> !(exe_reg_op1_data === exe_reg_op2_data),
      (exe_reg_exe_fun === BR_BLT) -> (exe_reg_op1_data.asSInt < exe_reg_op2_data.asSInt),
      (exe_reg_exe_fun === BR_BGE) -> !(exe_reg_op1_data.asSInt < exe_reg_op2_data.asSInt),
      (exe_reg_exe_fun === BR_BLTU) -> (exe_reg_op1_data < exe_reg_op2_data),
      (exe_reg_exe_fun === BR_BGEU) -> !(exe_reg_op1_data < exe_reg_op2_data)
    )
  )

  exe_br_target := exe_reg_pc + exe_reg_imm_b_sext

  exe_jmp_flg := (exe_reg_wb_sel === WB_PC)

  // ********** Execute/Memory (EX/MEM) Stage ***********
  mem_reg_pc := exe_reg_pc
  mem_reg_op1_data := exe_reg_op1_data
  mem_reg_rs2_data := exe_reg_rs2_data
  mem_reg_wb_addr := exe_reg_wb_addr
  mem_reg_alu_out := exe_alu_out
  mem_reg_rf_wen := exe_reg_rf_wen
  mem_reg_wb_sel := exe_reg_wb_sel
  mem_reg_csr_cmd := exe_reg_csr_cmd
  mem_reg_csr_addr := exe_reg_csr_addr
  mem_reg_imm_z_uext := exe_reg_imm_z_uext
  mem_reg_mem_wen := exe_reg_mem_wen

  // ********* Memory (MEM) Stage *********
  io.dmem.addr := mem_reg_alu_out
  io.dmem.wen := mem_reg_mem_wen
  io.dmem.wdata := mem_reg_rs2_data

  val csr_rdata = csr_regfile(mem_reg_csr_addr)
  val csr_wdata = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (mem_reg_csr_cmd === CSR_W) -> mem_reg_op1_data,
      (mem_reg_csr_cmd === CSR_S) -> (csr_rdata | mem_reg_op1_data),
      (mem_reg_csr_cmd === CSR_C) -> (csr_rdata & (~mem_reg_op1_data)),
      (mem_reg_csr_cmd === CSR_E) -> 11.U(WORD_LEN.W) // 机器模式的ECALL
    )
  )

  when(mem_reg_csr_cmd > 0.U) {
    csr_regfile(mem_reg_csr_addr) := csr_wdata
  }

  mem_wb_data := MuxCase(
    mem_reg_alu_out,
    Seq(
      (mem_reg_wb_sel === WB_MEM) -> io.dmem.rdata,
      (mem_reg_wb_sel === WB_PC) -> (mem_reg_pc + 4.U(WORD_LEN.W)),
      (mem_reg_wb_sel === WB_CSR) -> csr_rdata
    )
  )

  // ********** Memory/Write Back (MEM/WB) Stage ***********
  wb_reg_wb_data := mem_wb_data
  wb_reg_rf_wen := mem_reg_rf_wen
  wb_reg_wb_addr := mem_reg_wb_addr

  // ********* Write Back (WB) Stage *********
  when(wb_reg_rf_wen === REN_S) { regfile(wb_reg_wb_addr) := wb_reg_wb_data }

  // ********* Debugging *********
  io.gp := regfile(3)
  io.exit := (id_reg_inst === UNIMP)
  printf(p"---------------------\n")
  printf(p"if_reg_pc: 0x${Hexadecimal(if_reg_pc)}\n")
  printf(p"id_reg_pc: 0x${Hexadecimal(id_reg_pc)}\n")
  printf(p"id_reg_inst: 0x${Hexadecimal(id_reg_inst)}\n")
  printf(p"id_inst: 0x${Hexadecimal(id_inst)}\n")
  printf(p"id_rs1_data: 0x${Hexadecimal(id_rs1_data)}\n")
  printf(p"id_rs2_data: 0x${Hexadecimal(id_rs2_data)}\n")
  printf(p"exe_reg_pc: 0x${Hexadecimal(exe_reg_pc)}\n")
  printf(p"exe_reg_op1_data: 0x${Hexadecimal(id_op1_data)}\n")
  printf(p"exe_reg_op2_data: 0x${Hexadecimal(id_op2_data)}\n")
  printf(p"exe_alu_out: 0x${Hexadecimal(exe_alu_out)}\n")
  printf(p"mem_reg_pc: 0x${Hexadecimal(mem_reg_pc)}\n")
  printf(p"mem_wb_data: 0x${Hexadecimal(mem_wb_data)}\n")
  printf(p"wb_reg_wb_data: 0x${Hexadecimal(wb_reg_wb_data)}\n")
  printf(p"---------------------\n")

  // io.gp := regfile(3)
  // printf(p"---------------\n")
  // printf(
  //   p"io.pc: 0x${Hexadecimal(pc_reg)}\ninst: 0x${Hexadecimal(inst)} \n"
  // )
  // // printf(
  // //   p"rs1_addr: 0x${Hexadecimal(rs1_addr)} rs2_addr: 0x${Hexadecimal(rs2_addr)} wb_addr: 0x${Hexadecimal(wb_addr)}\n"
  // // )
  // // printf(p"gp: 0x${Hexadecimal(regfile(3))}\n")
  // printf(p"rs1_addr: 0x${Hexadecimal(rs1_addr)}\n")
  // // printf(p"rs2_data: 0x${Hexadecimal(rs2_data)}\n")
  // printf(p"wb_addr: 0x${Hexadecimal(wb_addr)}\n")
  // printf(p"rs1_data: 0x${Hexadecimal(rs1_data)}\n")
  // printf(p"wb_data: 0x${Hexadecimal(wb_data)}\n")
  // // printf(p"dmem.addr: 0x${Hexadecimal(io.dmem.addr)}\n")
  // // printf(p"dmem.wen: ${io.dmem.wen}\n")
  // // printf(p"dmem.wdata: 0x${Hexadecimal(io.dmem.wdata)}\n")
  // printf(p"---------------\n")

}
