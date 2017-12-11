package caclTests

import cacl._
import cacl.sequence._
import chisel3._
import firrtl._
import org.scalatest._

// Builds implication a ##[1:2] b |-> ##1 c
class OverlappingImplicationWrapper extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val c = Input(Bool())
    val satisfied = Output(Bool())
  })
  val signals = Seq(io.a, io.b, io.c)
  val implication = Module(OverlappingImplication(
    signals = signals,
    antecedent = Seq(ExpressionSequence(io.a), VariableDelaySequence(1, 2), ExpressionSequence(io.b)),
    consequent = Seq(DelaySequence(1), ExpressionSequence(io.c))
  ))
  implication.io.data := Vec(signals)
  io.satisfied := implication.io.satisfied
}

class OverlappingImplicationSpec extends FlatSpec {
  "A module containing implication a ##[1:2] b |-> ##1 c" should "be created" in {
    Compiler.compile(() => new OverlappingImplicationWrapper())
  }
}
