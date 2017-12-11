package caclTests

import chisel3._
import cacl._
import cacl.assert
import cacl.Sequence._

class PastTest extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(8.W))
  })
  val inc = RegNext(io.in +% 1.U)
  assert(!reset.toBool |=> inc === Past(io.in) +% 1.U)
}

class PastSpec extends EquivBaseSpec {
  "Past" should "pass formal verification" in {
    assert(checkAsserts(new PastTest))
  }
}

