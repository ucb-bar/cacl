package cacl

import chisel3._
import chisel3.util.{Cat, ValidIO}

class ModularSequenceInterface(val nSignals: Int) extends Bundle {
  val invoke = Input(Bool())
  val busy = Output(Bool())
  val matches = ValidIO(Bool())
  val data = Input(Vec(nSignals, Bool()))
  override def cloneType: this.type = new ModularSequenceInterface(nSignals).asInstanceOf[this.type]
}

abstract class ModularSequence extends Module {
  val io: ModularSequenceInterface
}

object EmptySequence {
  def apply(signals: Seq[Bool])() = new EmptySequence(signals)
}

class EmptySequence(signals: Seq[Bool]) extends ModularSequence {
  val io = IO(new ModularSequenceInterface(signals.size))
  io.busy := false.B
  io.matches.valid := io.invoke
  io.matches.bits := io.invoke
}

object ExpressionSequence {
  def apply(exp: Bool)(signals: Seq[Bool])(genChild: () => ModularSequence)() = new ExpressionSequence(exp, signals, genChild)
}

class ExpressionSequence(exp: Bool, signals: Seq[Bool], genChild: () => ModularSequence) extends ModularSequence {
  val io = IO(new ModularSequenceInterface(signals.size))
  val child = Module(genChild())
  child.io.invoke := io.invoke && io.data(signals.indexOf(exp))
  child.io.data := io.data
  io.busy := child.io.busy
  io.matches.valid := child.io.matches.valid
  io.matches.bits := child.io.matches.bits
}

object DelaySequence {
  def apply(nCycles: Int)(signals: Seq[Bool])(genChild: () => ModularSequence)() = new DelaySequence(nCycles, signals, genChild)
}

class DelaySequence(nCycles: Int, signals: Seq[Bool], genChild: () => ModularSequence) extends ModularSequence {
  val io = IO(new ModularSequenceInterface(signals.size))
  val child = Module(genChild())
  val invokeDelayed = RegInit(UInt(nCycles.W), 0.U)
  invokeDelayed := Cat(io.invoke, invokeDelayed >> 1)
  child.io.invoke := invokeDelayed(0)
  child.io.data := io.data
  io.busy := child.io.busy || invokeDelayed.orR
  io.matches.valid := child.io.matches.valid
  io.matches.bits := child.io.matches.bits
}

object VariableDelaySequence {
  def apply(minCycles: Int, maxCycles: Int)(signals: Seq[Bool])(genChild: () => ModularSequence)() = new VariableDelaySequence(minCycles, maxCycles, signals, genChild)
}

class VariableDelaySequence(minCycles: Int, maxCycles: Int, signals: Seq[Bool], genChild: () => ModularSequence) extends ModularSequence {
  val io = IO(new ModularSequenceInterface(signals.size))
  val nReplicas = maxCycles - minCycles + 1
  val children = Seq.fill(nReplicas){ Module(genChild()) }
  val invokeDelayed = RegInit(UInt(maxCycles.W), 0.U)
  invokeDelayed := Cat(io.invoke, invokeDelayed >> 1)
  children.zipWithIndex.foreach({ case (c, i) =>
    c.io.invoke := invokeDelayed(i)
    c.io.data := io.data
  })
  io.busy := Cat(children.map(_.io.busy)).orR || invokeDelayed.orR
  io.matches.valid := Cat(children.map(_.io.matches.valid)).orR
  io.matches.bits := Cat(children.map(_.io.matches.bits)).orR
}

object SequenceBuilder {
  type SequenceSignals = Seq[Bool]
  type BoundSequenceBuilder = (() => ModularSequence)
  type BoundSignalsSequenceBuilder = BoundSequenceBuilder => BoundSequenceBuilder
  type UnboundSequenceBuilder = SequenceSignals => BoundSignalsSequenceBuilder
  private def bindSignals(sb: UnboundSequenceBuilder, signals: SequenceSignals) = sb(signals)
  private def bindChild(outer: BoundSignalsSequenceBuilder, inner: BoundSequenceBuilder) = outer(inner)
  def apply(signals: SequenceSignals, sequences: UnboundSequenceBuilder*): BoundSequenceBuilder = {
    val boundToSignals = sequences.map(bindSignals(_, signals))
    boundToSignals.foldRight[BoundSequenceBuilder](EmptySequence(signals))(bindChild)
  }
}
