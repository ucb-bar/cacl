package caclTests

import cacl._
import cacl.sequence._
import chisel3._
import firrtl._
import org.scalatest._

// Builds sequence a ##[1:2] b ##1 c
class ModularSequenceWrapper extends SequenceImpl(3) {
  def seqBuilder = SequenceBuilder(
    signals,
    ExpressionSequence(io.in(0)),
    VariableDelaySequence(1, 2),
    ExpressionSequence(io.in(1)),
    DelaySequence(1),
    ExpressionSequence(io.in(2))
  )
}

// Direct implementation of a ##[1:2] b ##1 c
class A_Delay1to2_B_Delay1_C extends Module {
  val io = IO(new SequenceIntf(3))
  val sawA = RegNext(io.in(0), false.B)
  val sawA2 = RegNext(sawA, false.B)
  val sawAthenB = RegNext(false.B, false.B)
  when (sawA || sawA2) {
    when (io.in(1)) {
      sawAthenB := true.B
    }
  }
  io.seqMatch := false.B
  when (sawAthenB) {
    io.seqMatch := io.in(2)
  }
}

class ModularSequenceSpec extends EquivBaseSpec {
  "a ##[1:2] b ##1 c" should "be equivalent to handwritten FSM" in {
    assert(checkEquiv(new ModularSequenceWrapper, new A_Delay1to2_B_Delay1_C ))
  }
}
