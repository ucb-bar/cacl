package caclTests

import chisel3._
import chisel3.util.RegEnable
import cacl._
import cacl.assert
import cacl.Sequence._

class StableTest extends Module {
  val io = IO(new Bundle {
    val set = Input(Bool())
    val value = Input(UInt(8.W))
  })
  val reg = RegEnable(io.value, io.set)
  assert(!io.set |=> Stable(reg))
}

class StableSpec extends EquivBaseSpec {
  "Stable" should "pass formal verification" in {
    assert(checkAsserts(new StableTest))
  }
}

