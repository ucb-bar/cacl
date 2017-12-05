package cacl

import chisel3._
import scala.collection.mutable

abstract class Condition

case object Unconditional extends Condition

case class BoolCondition(cond: Bool) extends Condition

abstract class MatcherState {
  val cond: Condition
  val nextTrue: MatcherState
  val nextFalse: MatcherState
}

case class CheckState(cond: BoolCondition, nextTrue: MatcherState, nextFalse: MatcherState) extends MatcherState

case class StartState(cond: BoolCondition, nextTrue: MatcherState) extends MatcherState {
  val nextFalse = this
}

case class DelayState(next: MatcherState) extends MatcherState {
  val cond = Unconditional
  val nextTrue = next
  val nextFalse = next
}

case object MatchState extends MatcherState {
  val cond = Unconditional
  val nextTrue = this
  val nextFalse = this
}

case object FailState extends MatcherState {
  val cond = Unconditional
  val nextTrue = this
  val nextFalse = this
}

case object StateChain {
  def apply(delay: Delay, exp: ExpressionTerm, successor: MatcherState): MatcherState = {
    val cond = BoolCondition(exp.e)
    val states = new mutable.Stack[MatcherState]
    states.push(CheckState(cond, successor, FailState))
    for (max <- delay.maxCycles) {
      for (i <- delay.minCycles to max) {
        states.push(CheckState(cond, successor, states.head))
      }
    }
    for (i <- 1 to delay.minCycles) {
      states.push(DelayState(states.head))
    }
    states.head
  }

  def apply(s: SequenceChain): MatcherState = {
    val pairs = (s.seqs.drop(1) zip s.seqs.drop(2)).grouped(2).map(_.head)
    val continuation = pairs.foldRight[MatcherState](MatchState)({
      case ((d: Delay, e: ExpressionTerm), s: MatcherState) => StateChain(d, e, s)
    })
    s.seqs.head match {
      case exp: ExpressionTerm => StartState(BoolCondition(exp.e), continuation) 
    }
  }
}

abstract class FSM {
  val success: Bool
  val failure: Bool
}

class NaiveConcreteFSM(start: MatcherState) {
  private val stateLabels = new mutable.LinkedHashMap[MatcherState, UInt]
  private def freshIdx = stateLabels.size.U
  private def explore(s: MatcherState) {
    if (!stateLabels.contains(s)) {
      stateLabels += (s -> freshIdx)
      explore(s.nextTrue)
      explore(s.nextFalse)
    }
  }
  private def getCond(s: MatcherState) = s.cond match {
    case BoolCondition(c) => c
    case Unconditional => true.B
  }
  private def chooseNext(s: MatcherState) = s.cond match {
    case BoolCondition(c) => Mux(c, stateLabels(s.nextTrue), stateLabels(s.nextFalse))
    case Unconditional => stateLabels(s.nextTrue)
  }
  explore(start)

  // Remove psuedostates, replace with start
  stateLabels(FailState) = stateLabels(start)
  stateLabels(MatchState) = stateLabels(start)

  val currentState = RegInit(init = stateLabels(start))
  val nextState = Wire(UInt())
  val startWhen = when(currentState === stateLabels(start)) {
    currentState := chooseNext(start) }
  val condBlocks = stateLabels.tail.foldLeft(startWhen)({ case (prevCond, (state, label)) =>
    prevCond.elsewhen(currentState === label) { currentState := chooseNext(state) }})
  condBlocks.otherwise {
    nextState := stateLabels(start)
  }

  val successCheckState = stateLabels.find({ case (k, v) => k.nextTrue == MatchState }).get
  val success = (currentState === successCheckState._2) && getCond(successCheckState._1)
  val failure = false.B
}

