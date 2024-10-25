package `lw`

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._

class Core extends Module {
  val io = IO(new Bundle {
    val imem = Flipped(new ImemPortIo())
    val dmem = Flipped(new DmemPortIo())
    val exit = Output(Bool())
  })

  // * 生成32位x32个寄存器
  val regfile = Mem(32, UInt(WORD_LEN.W))
  // ********* Instruction Fetch (IF) Stage *********

  // * 生成初始值为0的PC寄存器,每个循环计数器递增4,STAR_ADDR=0

  val pc_reg = RegInit(START_ADDR)
  pc_reg := pc_reg + 4.U(WORD_LEN.W)

  // * 将pc_reg连接至输出端口addr端口,用inst连接输入端口inst
  io.imem.addr := pc_reg
  val inst = io.imem.inst

  // ********* Decode (ID) Stage *********
  val rs1_addr = inst(19, 15) //  rs1_addr为指令的rs1字段
  val rs2_addr = inst(24, 20) //  rs2_addr为指令的rs2字段
  val wb_addr = inst(11, 7) //  rd_addr为指令的rd字段
  val rs1_data =
    Mux((rs1_addr =/= 0.U(WORD_LEN.W)), regfile(rs1_addr), 0.U(WORD_LEN.W))
  val rs2_data =
    Mux((rs2_addr =/= 0.U(WORD_LEN.W)), regfile(rs2_addr), 0.U(WORD_LEN.W))
  val imm_i = inst(31, 20) //  imm_i为指令的立即数字段
  val imm_i_sext = Cat(Fill(20, imm_i(11)), imm_i)

  // ********* Execute (EX) Stage *********
  val alu_out =
    MuxCase(0.U(WORD_LEN.W), Seq((inst === LW) -> (rs1_data + imm_i_sext)))

  // ********* Memory (MEM) Stage *********
  io.dmem.addr := alu_out

  // ********* Write Back (WB) Stage *********
  val wb_data = io.dmem.rdata
  when(inst === LW) { regfile(wb_addr) := wb_data }

  io.exit := (inst === 0x14131211.U(WORD_LEN.W))

  // ********* Debugging *********
  printf(p"---------------\n")
  printf(
    p"pc: 0x${Hexadecimal(pc_reg)} inst: 0x${Hexadecimal(inst)} \n"
  )
  printf(
    p"rs1_addr: 0x${Hexadecimal(rs1_addr)} rs2_addr: 0x${Hexadecimal(rs2_addr)} wb_addr: 0x${Hexadecimal(wb_addr)}\n"
  )
  printf(p"rs1_data: 0x${Hexadecimal(rs1_data)}\n")
  printf(p"rs2_data: 0x${Hexadecimal(rs2_data)}\n")
  printf(p"wb_data: 0x${Hexadecimal(wb_data)}\n")
  printf(p"dmem.addr: 0x${Hexadecimal(io.dmem.addr)}\n")
  printf(p"---------------\n")

}
