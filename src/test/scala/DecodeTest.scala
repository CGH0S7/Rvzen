package decode

import chisel3._
import org.scalatest.flatspec._
import chiseltest._

class HexTest extends AnyFlatSpec with ChiselScalatestTester {
  "Rvzen" should "decode hex instruction" in {
    test(new Top) { c =>
      while (!c.io.exit.peek().litToBoolean) { c.clock.step(1) }
    }
  }
}
