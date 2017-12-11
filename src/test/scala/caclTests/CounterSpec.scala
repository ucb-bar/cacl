package caclTests

import chisel3._
import cacl._
import cacl.assert
import cacl.Sequence._

/** Up-down, saturating, loadable Counter
  *
  * Example adapted from Counter example on pg. 375-385
  * "The Art of Verification with SystemVerilog Assertions" by Hague, Michelson, and Khan
  */
class Counter(width: Int) extends Module {
  require(width > 1, "width of Counter must be greater than 1!")
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
  assert(!do_inc || (count =/= ("b" + "1"*width).U))
  // Check if decrement is about to underflow counter
  assert(!do_dec || (count =/= 0.U))
  // Reset and clear zero the counter
  assert((reset.toBool || io.clear) |=> (count === 0.U))
  // Load behaves correctly
  assert((io.load && !io.clear) |=> (count === Past(io.din)))
  // The counter increments correctly
  assert((io.inc && !io.load && !io.clear && !io.dec && !saturated) |=>
           (count === (Past(count) + 1.U)))
  // The counter does not overflow except during a load or clear
  assert((!io.load && !io.clear && (count === ("b" + "1"*width).U)) |=> (count =/= 0.U))
  // The counter decrements correctly
  assert((io.dec && !io.load && !io.clear && !io.inc && !zeroed) |=>
          (count === (Past(count) - 1.U)))
  // The counter does not underflow except during a load
  assert((!io.load && (count === 0.U)) |=> (count =/= ("b" + "1"*width).U))
  // The counter is stable when it should not change
  assert((!io.load && !io.clear && ((!io.inc && !io.dec) || (io.inc && io.dec))) |=>
           Stable(count))
}

class CounterSpec extends EquivBaseSpec {
  "Counter" should "pass formal verification" in {
    for (width <- 2 to 10) {
      assert(checkAsserts(new Counter(width)))
    }
  }
}

