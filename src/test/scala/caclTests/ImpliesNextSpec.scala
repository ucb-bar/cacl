package caclTests

import chisel3._

// s1 |=> s2
class ImpliesNextCycle extends PropImpl(2) {
  val doCheck = RegNext(in(0))

  holds := true.B
  when (doCheck) {
    holds := in(1)
  }

  def verilogImpl = """
module gold(
  input clock,
  input reset,
  input in_0,
  input in_1,
  output holds
);
  reg do_check;

  assign holds = do_check ? in_1 : 1;

  always @(posedge clock) begin
    do_check <= in_0;
  end
endmodule
""".stripMargin
}

class ImpliesNextSpec extends EquivBaseSpec {
  "s1 |=> s2" should "match Verilog specification" in {
    assert(checkEquiv(new ImpliesNextCycle))
  }
}

