package `decode`

import chisel3._
import chisel3.util._
import common.Consts._

class Core extends Module {
  val io = IO(new Bundle {
    // * 生成输出端口addr和输入端口inst
    val imem = Flipped(new ImemPortIo())
    // * 输出端口exit在程序处理结束时变为true.B
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
  val rs1_addr = inst(19, 15) // * rs1_addr为指令的rs1字段
  val rs2_addr = inst(24, 20) // * rs2_addr为指令的rs2字段
  val wb_addr = inst(11, 7) // * rd_addr为指令的rd字段
  val rs1_data =
    Mux((rs1_addr =/= 0.U(WORD_LEN.W)), regfile(rs1_addr), 0.U(WORD_LEN.W))
  val rs2_data =
    Mux((rs2_addr =/= 0.U(WORD_LEN.W)), regfile(rs2_addr), 0.U(WORD_LEN.W))

  // * 当inst为"44434241"是设定exit为ture.B
  io.exit := (inst === 0x44434241.U(WORD_LEN.W))

  // ********* Debugging *********
  printf(p"---------------\n")
  printf(
    p"pc: 0x${Hexadecimal(pc_reg)} inst: 0x${Hexadecimal(inst)} \n"
  )
  printf(p"rs1_addr: ${rs1_addr} rs2_addr: ${rs2_addr} wb_addr: ${wb_addr}\n")
  printf(p"rs1_data: 0x${Hexadecimal(rs1_data)}\n")
  printf(p"rs2_data: 0x${Hexadecimal(rs2_data)}\n")
  printf(p"---------------\n")

}
