package cacl

import chisel3._
import scala.language.implicitConversions

object Sequence {
  def apply(seqs: Sequence*): Sequence = SequenceChain(seqs)
  implicit def bool2Sequence(e: Bool): Sequence = ExpressionTerm(e)
}

abstract class Sequence {
  def |->(rhs: Sequence): Implication = Implication(this, rhs)
  def |=>(rhs: Sequence): Implication = Implication(this, Sequence(Delay(1), rhs))
}

case class ExpressionTerm(e: Bool) extends Sequence

object Past {
  def apply[T <: Data](data: T): T = RegNext(data)
}
object Stable {
  def apply[T <: Data](data: T): Bool = data.asUInt === Past(data).asUInt
}

case class Delay(minCycles: Int, maxCycles: Option[Int] = None) extends Sequence
object Delay {
  def apply(minCycles: Int, maxCycles: Int) = new Delay(minCycles, Some(maxCycles))
}

case class Repeat(s: Sequence, min: Int, max: Option[Int] = None) extends Sequence
object Repeat {
  def apply(s: Sequence, min: Int, max: Int) = new Repeat(s, min, Some(max))
}

case class SequenceChain(seqs: Seq[Sequence]) extends Sequence

abstract class Property

case class Implication(lhs: Sequence, rhs: Sequence) extends Property
