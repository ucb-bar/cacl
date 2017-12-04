package caclTests

import chisel3._
import chisel3.util.{HasBlackBoxInline}
import chisel3.experimental._
import firrtl.util.BackendCompilationUtilities

import scala.sys.process._

// s1 | => s2
class ImpliesNextCycle extends Module {
  val io = IO(new Bundle {
    val s1 = Input(Bool())
    val s2 = Input(Bool())
    val fail = Output(Bool())
  })
  val check = RegNext(io.s1)
  when (check) {
    io.fail := !io.s2
  } .otherwise {
    io.fail := false.B
  }
}

class ImpliesNextCycleGoldImpl extends HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val io_s1 = Input(Bool())
    val io_s2 = Input(Bool())
    val io_fail = Output(Bool())
  })
  setInline("gold.v","""
   |module gold(
   |  input clock,
   |  input reset,
   |  input io_s1,
   |  input io_s2,
   |  output io_fail
   |);
   |  reg check;
   |  wire next_check;
   |
   |  assign next_check = io_s1 ? 1 : 0;
   |  assign io_fail = check ? ~io_s2 : 0;
   |
   |  always @(posedge clock) begin
   |    check <= next_check;
   |  end
   |endmodule
   |""".stripMargin)
}

class ImpliesNextCycleGold extends Module {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val s1 = Input(Bool())
    val s2 = Input(Bool())
    val fail = Output(Bool())
  })
  val impl = Module(new ImpliesNextCycleGoldImpl)
  impl.io.clock := io.clock
  impl.io.reset := io.reset
  impl.io.io_s1 := io.s1
  impl.io.io_s2 := io.s2
  io.fail := impl.io.io_fail
}

object TestImpliesNext extends App with BackendCompilationUtilities {

  val testDir = createTestDirectory("TestImplies")
  chisel3.Driver.execute(Array("-td", testDir.toString), () => new ImpliesNextCycle)
  chisel3.Driver.execute(Array("-td", testDir.toString), () => new ImpliesNextCycleGold)

  val command = Seq(
    "yosys",
    "-p",
    s"""|
    |read_verilog $testDir/ImpliesNextCycle.v
    |read_verilog $testDir/gold.v
    |prep; proc; opt; memory
    |miter -equiv -flatten ImpliesNextCycle gold miter
    |hierarchy -top miter
    |sat -verify -tempinduct -prove trigger 0 -seq 10 miter
    |""".stripMargin
  )
  println(command.mkString(" "))
  val res = command.!
  println(s"res = $res")
}
