package caclTests

import cacl._
import cacl.sequence._
import chisel3._

// Builds sequence a ##1 b
class IdentitySequenceWrapper extends SequenceImpl(1) {
  def seqBuilder = SequenceBuilder(
    signals,
    ExpressionSequence(io.in(0))
  )
}

// Direct implementation of a ##1 b
class IdentityRefImpl extends Module {
  val io = IO(new SequenceIntf(1))
  io.seqMatch := io.in(0)
}

class IdentitySequenceSpec extends EquivBaseSpec {
  "The identity" should "be equivalent to a handwritten FSM" in {
    assert(checkEquiv(new IdentitySequenceWrapper, new IdentityRefImpl))
  }
}

