package caclTests

import chisel3._

// s1 ##1 s2
class DelayTest extends PropImpl(2) {
  val prev = RegNext(in(0))
  holds := prev && in(1)

  def verilogImpl = """
module gold(
  input clock,
  input reset,
  input in_0,
  input in_1,
  output holds
);
  reg prev;

  assign holds = prev && in_1;

  always @(posedge clock) begin
    prev <= in_0;
  end
endmodule
""".stripMargin
}

class DelaySpec extends EquivBaseSpec {
  "s1 ##1 s2" should "match Verilog specification" in {
    assert(checkEquiv(new DelayTest))
  }
}

