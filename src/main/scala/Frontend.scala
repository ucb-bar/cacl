package cacl

import chisel3._
import scala.language.implicitConversions

object Sequence {
  def apply(seqs: Sequence*): Sequence = SequenceChain(seqs)
  implicit def bool2Sequence(e: Bool): Sequence = ExpressionTerm(e)
}

abstract class Sequence {
  def |->(rhs: Sequence): Implication = Implication(this, rhs)
}

case class ExpressionTerm(e: Bool) extends Sequence

case class Delay(minCycles: Int, maxCycles: Option[Int] = None) extends Sequence

case class Repeat(s: Sequence, min: Int, max: Option[Int] = None) extends Sequence

case class SequenceChain(seqs: Seq[Sequence]) extends Sequence

case class Implication(lhs: Sequence, rhs: Sequence)
