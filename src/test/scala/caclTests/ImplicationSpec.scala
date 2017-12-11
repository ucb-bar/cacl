package caclTests

import cacl._
import cacl.sequence._
import chisel3._
import firrtl._
import org.scalatest._

// Builds implication a ##[1:2] b |-> ##1 c
class OverlappingImplicationWrapper extends PropertyImpl(3) {
  def implicationGen = () => OverlappingImplication(
    signals = signals,
    antecedent = Seq(ExpressionSequence(io.in(0)), VariableDelaySequence(1, 2), ExpressionSequence(io.in(1))),
    consequent = Seq(DelaySequence(1), ExpressionSequence(io.in(2)))
  )
}

class OverlappingImplicationRefImpl extends Module {
  val io = IO(new PropertyIntf(3))
  val sawA = RegNext(io.in(0), false.B)
  val sawA2 = RegNext(sawA, false.B)

  val sawAandB = RegNext((sawA || sawA2) && io.in(1), false.B)

  io.satisfied := true.B
  when (sawAandB) {
    io.satisfied := io.in(2)
  }
}

class OverlappingImplicationSpec extends EquivBaseSpec {
  "a ##[1:2] b |-> ##1 c" should "be equivalent to handwritten FSM" in {
    assert(checkEquiv(new OverlappingImplicationWrapper, new OverlappingImplicationRefImpl  ))
  }
}
