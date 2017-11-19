package caclTests

import org.scalatest._
import cacl._
import chisel3._

class FrontendSpec extends FlatSpec {
  "An Implication" should "be created" in {
    Sequence(true.B, Delay(1), true.B) |-> Sequence(true.B)
  }
}
