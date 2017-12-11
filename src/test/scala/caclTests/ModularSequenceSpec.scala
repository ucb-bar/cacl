package caclTests

import cacl._
import cacl.sequence._
import chisel3._
import firrtl._
import org.scalatest._

// TODO (amagyar): import test code from chisel3; doesn't seem to be in jar file :(
object Compiler {
  def compile(t: () => Module): Unit = {
    val manager = new ExecutionOptionsManager("compile") with HasFirrtlOptions
        with HasChiselExecutionOptions {
      commonOptions = CommonOptions(targetDirName = "test_run_dir")
    }

    chisel3.Driver.execute(manager, t)
  }
}

// Builds sequence a ##[1:2] b ##1 c
class ModularSequenceWrapper extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val c = Input(Bool())
    val sequenceMatch = Output(Bool())
  })
  val signals = Seq(io.a, io.b, io.c)
  val topSequenceBuilder = SequenceBuilder(
    signals,
    ExpressionSequence(io.a),
    VariableDelaySequence(1, 2),
    ExpressionSequence(io.b),
    DelaySequence(1),
    ExpressionSequence(io.c)
  )
  val topSequence = Module(topSequenceBuilder())
  topSequence.io.invoke := true.B
  topSequence.io.data := Vec(signals)
  io.sequenceMatch := topSequence.io.matches.valid && topSequence.io.matches.bits
}

class ModularSequenceSpec extends FlatSpec {
  "A module containing a modular CACL sequence" should "be created" in {
    Compiler.compile(() => new ModularSequenceWrapper())
  }
}
