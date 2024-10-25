package r12426im

import chisel3._
import org.scalatest.flatspec._
import chiseltest._

class pipelineTest extends AnyFlatSpec with ChiselScalatestTester {
  "Rvzen" should "run the C program" in {
    test(new Top) { c =>
      while (!c.io.exit.peek().litToBoolean) { c.clock.step(1) }
    }
  }
}
