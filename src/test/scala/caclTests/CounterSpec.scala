package caclTests

import chisel3._
import caclTests.utils.realassert

class Counter(width: Int) extends Module {
  val io = IO(new Bundle {
    val clear = Input(Bool())
    val load = Input(Bool())
    val din = Input(UInt(width.W))
    val inc = Input(Bool())
    val dec = Input(Bool())
    val count = Output(UInt(width.W))
  })

  val count = RegInit(UInt(width.W), 0.U)
  io.count := count
  val saturated = count === ((1 << width) - 1).U
  val zeroed = count === 0.U

  // Workaround wires for getting full assertion predicate when nested in when block
  // See https://github.com/freechipsproject/chisel3/issues/726
  val do_inc = WireInit(false.B)
  val do_dec = WireInit(false.B)

  when (io.clear) {
    count := 0.U
  } .elsewhen (io.load) {
    count := io.din
  } .elsewhen (io.inc && !io.dec && !saturated) {
    do_inc := true.B
    count := count +% 1.U
  } .elsewhen (io.dec && !io.inc && !zeroed) {
    do_dec := true.B
    count := count -% 1.U
  }
  // Check if increment is about to overflow count
  realassert(!do_inc || (count =/= ("b" + "1"*width).U))
  // Check if decrement is about to underflow counter
  realassert(!do_dec || (count =/= 0.U))
}

class CounterSpec extends EquivBaseSpec {
  "Counter" should "pass formal verification" in {
    assert(checkAsserts(new Counter(8)))
  }
}

