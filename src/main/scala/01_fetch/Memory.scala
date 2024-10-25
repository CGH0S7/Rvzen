package `fetch`

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
import common.Consts._
import chisel3.util.experimental.loadMemoryFromFileInline

//  ImemPortIo类继承自Bundle类, 捆绑addr与inst两个信号。
//     addr: 存储器地址用输入端口
//     inst: 指令数据用输出端口
//     均为32位宽。

class ImemPortIo extends Bundle {
  val addr = Input(UInt(WORD_LEN.W))
  val inst = Output(UInt(WORD_LEN.W))
}

class Memory extends Module {
  val io = IO(new Bundle {
    val imem = new ImemPortIo()
  })

  // 生成八位宽x16384(16kb)寄存器的指令存储器。
  val mem = Mem(16384, UInt(8.W))

  // 加载指令存储器的初始值。
  // loadMemoryFromFile(mem, "src/hex/fetch.hex")
  loadMemoryFromFileInline(mem, "src/hex/fetch.hex")

  // 连接四个地址存储的八位数据形成一个32位的指令。
  io.imem.inst := Cat(
    mem(io.imem.addr + 3.U(WORD_LEN.W)),
    mem(io.imem.addr + 2.U(WORD_LEN.W)),
    mem(io.imem.addr + 1.U(WORD_LEN.W)),
    mem(io.imem.addr)
  )
}