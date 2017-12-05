package caclTests

import chisel3._

// s1 |=> s2
class ImpliesNextCycle extends PropImpl(2) {
  val check = RegNext(in(0))
  when (check) {
    fail := !in(1)
  } .otherwise {
    fail := false.B
  }
  def verilogImpl = """
module gold(
  input clock,
  input reset,
  input in_0,
  input in_1,
  output fail
);
  reg check;
  wire next_check;

  assign next_check = in_0 ? 1 : 0;
  assign fail = check ? ~in_1 : 0;

  always @(posedge clock) begin
    check <= next_check;
  end
endmodule
""".stripMargin
}

class ImpliesNextSpec extends EquivBaseSpec {
  "s1 |=> s2" should "work" in {
    assert(checkEquiv(new ImpliesNextCycle))
  }
}

