package `sw`

import chisel3._
import chisel3.util._

class Top extends Module {
  val io = IO(new Bundle {
    val exit = Output(Bool())
  })

  // * 先通过new实例化再通过Module硬件化
  val core = Module(new Core())
  val memory = Module(new Memory())

  // * core的io和memory的io是ImemPortIo翻转的关系用<>批量连接
  core.io.imem <> memory.io.imem
  core.io.dmem <> memory.io.dmem

  io.exit := core.io.exit
}
