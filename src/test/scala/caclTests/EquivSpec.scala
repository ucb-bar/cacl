package caclTests

import chisel3._
import chisel3.experimental._
import chisel3.core.Reset
import firrtl.transforms.{BlackBoxSourceHelper, BlackBoxInline}
import firrtl.util.BackendCompilationUtilities
import org.scalatest._

import cacl._
import cacl.sequence._

import java.io.{File, FileWriter}
import scala.sys.process._

class SequenceIntf(numInputs: Int) extends Bundle {
  require(numInputs > 0)
  val in = Input(Vec(numInputs, Bool()))
  val seqMatch = Output(Bool())
}

abstract class SequenceImpl(numInputs: Int) extends Module {
  val io = IO(new SequenceIntf(numInputs))

  def seqBuilder: BoundSequenceBuilder

  val signals = io.in
  val sequence = Module(seqBuilder())
  sequence.io.invoke := true.B
  sequence.io.data := signals
  io.seqMatch := sequence.io.matches.valid && sequence.io.matches.bits
}

class PropertyIntf(numInputs: Int) extends Bundle {
  require(numInputs > 0)
  val in = Input(Vec(numInputs, Bool()))
  val satisfied = Output(Bool())
}

abstract class PropertyImpl(numInputs: Int) extends Module {
  val io = IO(new PropertyIntf(numInputs))

  val signals = io.in
  def implicationGen: () => OverlappingImplication

  val implication = Module(implicationGen())
  implication.io.data := signals
  io.satisfied := implication.io.satisfied
}

abstract class EquivBaseSpec extends FlatSpec with BackendCompilationUtilities {
  def testName = this.getClass.getSimpleName

  private def writeToFile(content: String, directory: File, filename: String): File = {
    val f = new File(directory, filename)
    val w = new FileWriter(f)
    w.write(content)
    w.close()
    f
  }

  def compileModule[T <: RawModule](targetDir: File, mod: => T): (String, T) = 
    chisel3.Driver.execute(Array("-td", s"$targetDir"), () => mod) match {
      case ChiselExecutionSuccess(Some(cir),_,_) =>
        val topMod = cir.components.find(_.name == cir.name).get.id
        (cir.name, topMod.asInstanceOf[T])
    }

  /** Generates Verilog and runs Yosys to check equivalence of two Modules */
  def checkEquiv(a: => Module, b: => Module): Boolean = {

    val testDir = createTestDirectory(testName)

    val (aName: String, elabA: Module) = compileModule(testDir, a)
    val (bName: String, elabB: Module) = compileModule(testDir, b)
    val aFile = new File(testDir, s"$aName.v")
    val bFile = new File(testDir, s"$bName.v")

    val yosysScriptContents = s"""
      |read_verilog ${aFile.getAbsoluteFile}
      |read_verilog ${bFile.getAbsoluteFile}
      |prep; proc; opt; memory
      |miter -equiv -flatten $aName $bName miter
      |hierarchy -top miter
      |sat -verify -tempinduct -prove trigger 0 -set in_reset 0 -set-at 1 in_reset 1 -seq 1 miter
      |""".stripMargin
    val yosysScript = writeToFile(yosysScriptContents, testDir, "lec.ys")

    val command = Seq("yosys", "-q", "-s", s"$yosysScript")
    println(command.mkString(" "))
    command.! == 0
  }

  // Generates Verilog and runs Yosys to check assertions
  def checkAsserts(design: => Module): Boolean = {

    val testDir = createTestDirectory(testName)

    val (top: String, elabDesign: Module) =
      chisel3.Driver.execute(Array("-td", s"$testDir"), () => design) match {
        case ChiselExecutionSuccess(Some(cir),_,_) =>
          val topMod = cir.components.find(_.name == cir.name).get.id
          (cir.name, topMod)
        case other => throw new Exception("Unexpected failure!")
      }
    val designFile = new File(testDir, s"$top.v")

    val yosysScriptContents = s"""
      |read_verilog -sv ${testDir.getAbsolutePath}/*.v
      |prep; proc; opt; memory
      |hierarchy -top $top
      |flatten
      |sat -verify  -prove-asserts -tempinduct -set reset 0 -set-at 1 reset 1 -seq 1
      |""".stripMargin
    val yosysScript = writeToFile(yosysScriptContents, testDir, "lec.ys")

    val command = Seq("yosys", "-q", "-s", s"$yosysScript")
    println(command.mkString(" "))
    command.! == 0
  }
}

// Who checks the checkers?
final class EquivSpec extends EquivBaseSpec {
  class BrokenAssert extends Module {
    val io = IO(new Bundle {
      val a = Input(Bool())
      val b = Input(Bool())
    })
    cacl.assert(io.a === io.b)
  }
  "Falsifiable assertions" should "fail sat" in {
    assert(!checkAsserts(new BrokenAssert))
  }
}

