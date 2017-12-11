package cacl

import chisel3._

package object sequence {
  type SequenceSignals = Seq[Bool]
  type BoundSequenceBuilder = (() => ModularSequence)
  type BoundSignalsSequenceBuilder = BoundSequenceBuilder => BoundSequenceBuilder
  type UnboundSequenceBuilder = SequenceSignals => BoundSignalsSequenceBuilder
}
