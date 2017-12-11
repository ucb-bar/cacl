
package cacl

import chisel3._
import chisel3.experimental._
import chisel3.util.HasBlackBoxInline

import cacl.sequence._

object assert {
  class BlackBoxAssert extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
      val clock = Input(Clock())
      val reset = Input(Bool())
      val cond = Input(Bool())
    })
    override def desiredName = BlackBoxAssert.name
    setInline(s"${BlackBoxAssert.name}.v", s"""
      |module BlackBoxAssert (
      |  input clock,
      |  input reset,
      |  input cond
      |);
      |always @(posedge clock) begin
      |  if (!reset) assert (cond);
      |end
      |endmodule
      |""".stripMargin)
  }
  object BlackBoxAssert {
    def name = "BlackBoxAssert"
  }

  // Insert Verilog assert in a blackbox
  def apply(cond: Bool): Unit = {
    val mod = Module(new BlackBoxAssert)
    mod.io.clock := Module.clock
    mod.io.reset := Module.reset
    mod.io.cond := cond
  }
  def apply(property: Implication): Unit = {
    val (antSigs, antBuilders) = SequenceBuilder.expand(property.lhs)
    val (conSigs, conBuilders) = SequenceBuilder.expand(property.rhs)
    val signals = antSigs ++ conSigs
    val mod = Module(new OverlappingImplication(signals, antBuilders, conBuilders))
    mod.io.data := signals
    assert(mod.io.satisfied)
  }
  def apply(sequence: Sequence): Unit = {
    apply(Implication(ExpressionTerm(true.B), sequence))
  }
}
