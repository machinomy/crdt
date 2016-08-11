package com.machinomy.crdt.state

case class ORSet[E, T: TombStone](state: Map[E, ORSet.Moves[T]]) extends Convergent[E, Set[E]] {
  override type Self = ORSet[E, T]

  def +(element: E, tomb: T): Self = {
    val moves: ORSet.Moves[T] = state.getOrElse(element, ORSet.Moves[T]())
    val nextAdditions = moves.additions + tomb
    val nextMoves = moves.copy(additions = nextAdditions)
    copy(state = state.updated(element, nextMoves))
  }

  def +(pair: (E, T)): Self = this + (pair._1, pair._2)

  def +(element: E): Self = this + (element, implicitly[TombStone[T]].next)

  def -(element: E): Self = {
    val moves: ORSet.Moves[T] = state.getOrElse(element, ORSet.Moves[T]())
    val nextRemovals = moves.removals ++ moves.additions
    val nextMoves = moves.copy(removals = nextRemovals)
    copy(state = state.updated(element, nextMoves))
  }

  override def merge(other: Self): Self = ???

  override def value: Set[E] = state.keySet.filter { element =>
    val moves: ORSet.Moves[T] = state.getOrElse(element, ORSet.Moves[T]())
    (moves.additions -- moves.removals).nonEmpty
  }
}

object ORSet {
  case class Moves[T](additions: Set[T] = Set.empty[T], removals: Set[T] = Set.empty[T])

  def apply[E, T: TombStone](): ORSet[E, T] = ORSet[E, T](Map.empty[E, Moves[T]])
}
