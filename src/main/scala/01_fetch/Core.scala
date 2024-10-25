package `fetch`

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

  // * 当inst为"44434241"是设定exit为ture.B
  io.exit := (inst === 0x44434241.U(WORD_LEN.W))

  // ********* Debugging *********
  printf(p"---------------\n")
  printf(
    p"pc: 0x${Hexadecimal(pc_reg)} inst: 0x${Hexadecimal(inst)} \n"
  )
  printf(p"---------------\n")

}
