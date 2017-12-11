package cacl

import sequence._

import chisel3._
import chisel3.util.{Cat, ValidIO, Counter}
import scala.collection.mutable

class ImplicationInterface(val nSignals: Int) extends Bundle {
  val satisfied = Output(Bool())
  val data = Input(Vec(nSignals, Bool()))
}

object OverlappingImplication {
  def apply(signals: SequenceSignals, antecedent: Seq[UnboundSequenceBuilder], consequent: Seq[UnboundSequenceBuilder]) = new OverlappingImplication(signals, antecedent, consequent)
}

class OverlappingImplication(signals: SequenceSignals, antecedent: Seq[UnboundSequenceBuilder], consequent: Seq[UnboundSequenceBuilder]) extends Module {
  val io = IO(new ImplicationInterface(signals.size))
  val boundAnteBuilder = SequenceBuilder(signals, antecedent:_*)
  val boundConsBuilder = SequenceBuilder(signals, consequent:_*)

  val antecedentReps = mutable.ArrayBuffer(Module(boundAnteBuilder()))
  val consequentReps = mutable.ArrayBuffer(Module(boundConsBuilder()))

  val nReps = antecedentReps(0).maxTime + consequentReps(0).maxTime
  antecedentReps ++= Seq.fill(nReps){ Module(boundAnteBuilder()) }
  consequentReps ++= Seq.fill(nReps){ Module(boundConsBuilder()) }

  val invokeScheduler = Counter(nReps + 1)
  invokeScheduler.inc()
  // TODO (amagyar): check if any sequence is invoked while busy -> metassertion fail

  (antecedentReps zip consequentReps).zipWithIndex.foreach({ case ((a, c), i) =>
    a.io.invoke := invokeScheduler.value === i.U
    a.io.data := io.data
    c.io.invoke := a.io.matches.valid && a.io.matches.bits
    c.io.data := io.data
  })

  val violated = Cat(consequentReps.map(c => c.io.matches.valid && !c.io.matches.bits)).orR
  io.satisfied := !violated
}
