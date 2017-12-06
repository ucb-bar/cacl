package caclTests

import chisel3._

// s1 implies s2
class ImplicationTest extends PropImpl(2) {
  holds := !in(0) || in(1)

  def verilogImpl = """
module gold(
  input clock,
  input reset,
  input in_0,
  input in_1,
  output holds
);
  assign holds = !in_0 || in_1;
endmodule
""".stripMargin
}

class ImplicationSpec extends EquivBaseSpec {
  "s1 implies s2" should "match Verilog specification" in {
    assert(checkEquiv(new ImplicationTest))
  }
}

