package caclTests

import chisel3._
import cacl._
import cacl.assert
import cacl.Sequence._

class NegativeSequenceTest extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(8.W))
    val b = Input(UInt(8.W))
  })
  assert(io.b === Past(io.a))
}

class PositiveSequenceTest extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(8.W))
    val b = Input(UInt(8.W))
  })
  val child = Module(new NegativeSequenceTest)
  child.io.a := io.a
  child.io.b := RegNext(io.a)
}

class AssertSpec extends EquivBaseSpec {
  "asserts on sequences" should "fail formal verification if provably false" in {
    assert(!checkAsserts(new NegativeSequenceTest))
  }
  they should "pass formal verification if provably true" in {
    assert(checkAsserts(new PositiveSequenceTest))
  }
}

