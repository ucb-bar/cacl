
package cacl

import chisel3._
import chisel3.experimental._
import chisel3.util.HasBlackBoxInline

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
  def apply(cond: Bool) = {
    val mod = Module(new BlackBoxAssert)
    mod.io.clock := Module.clock
    mod.io.reset := Module.reset
    mod.io.cond := cond
  }
}
