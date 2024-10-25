package riscvtests

import chisel3._
import org.scalatest.flatspec._
import chiseltest._

class RiscvTest extends AnyFlatSpec with ChiselScalatestTester {
  "Rvzen" should "work through hex" in {
    test(new Top) { c =>
      while (!c.io.exit.peek().litToBoolean) { c.clock.step(1) }
      c.clock.step(1)
      c.io.gp.expect(1.U)
    }
  }
}
