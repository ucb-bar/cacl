package caclTests

import chisel3._
import chisel3.experimental._
import chisel3.core.Reset
import firrtl.transforms.{BlackBoxSourceHelper, BlackBoxInline}
import firrtl.util.BackendCompilationUtilities
import org.scalatest._

import java.io.{File, FileWriter}
import scala.sys.process._

sealed trait PropImplIntf extends BaseModule {
  def clock: Clock
  def reset: Reset
}

abstract class PropImpl(numInputs: Int) extends MultiIOModule {
  require(numInputs > 0)
  val in = IO(Input(Vec(numInputs, Bool())))
  val holds = IO(Output(Bool()))
  def verilogImpl: String
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

  // Generates Verilog and runs Yosys to check equivalence of two PropImpls
  def checkEquiv(design: => PropImpl): Boolean = {

    val testDir = createTestDirectory(testName)

    val (top: String, elabDesign: PropImpl) =
      chisel3.Driver.execute(Array("-td", s"$testDir"), () => design) match {
        case ChiselExecutionSuccess(Some(cir),_,_) =>
          val topMod = cir.components.find(_.name == cir.name).get.id
          (cir.name, topMod)
      }
    val designFile = new File(testDir, s"$top.v")
    val gold: File = writeToFile(elabDesign.verilogImpl, testDir, "gold.v")

    val yosysScriptContents = s"""
      |read_verilog ${designFile.getAbsoluteFile}
      |read_verilog ${gold.getAbsoluteFile}
      |prep; proc; opt; memory
      |miter -equiv -flatten $top gold miter
      |hierarchy -top miter
      |sat -verify -tempinduct -prove trigger 0 -seq 1 miter
      |""".stripMargin
    val yosysScript = writeToFile(yosysScriptContents, testDir, "lec.ys")

    val command = Seq("yosys", "-s", s"$yosysScript")
    println(command.mkString(" "))
    command.! == 0
  }
}
