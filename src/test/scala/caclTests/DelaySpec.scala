package caclTests

import cacl._
import cacl.sequence._
import chisel3._

// Builds sequence a ##1 b
class DelayOneSequenceWrapper extends PropImpl(2) {
  def seqBuilder = SequenceBuilder(
    signals,
    ExpressionSequence(io.in(0)),
    DelaySequence(1),
    ExpressionSequence(io.in(1))
  )
}

// Direct implementation of a ##1 b
class DelayOneRefImpl extends Module {
  val io = IO(new PropIntf(2))
  val prev = RegNext(io.in(0))
  io.seqMatch := prev && io.in(1)
}

class DelayOneSequenceSpec extends EquivBaseSpec {
  "a ##1 b" should "be equivalent to a handwritten FSM" in {
    assert(checkEquiv(new DelayOneSequenceWrapper, new DelayOneRefImpl))
  }
}

