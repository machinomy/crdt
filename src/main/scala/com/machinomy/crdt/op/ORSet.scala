package com.machinomy.crdt.op

import com.machinomy.crdt.state.TombStone

case class ORSet[E, T: TombStone](pairs: Set[(E, T)]) {
  lazy val value: Set[E] = pairs.map(_._1)

  def add(e: E): ORSet.UpdateResult[E, T] = {
    val t = implicitly[TombStone[T]].next
    val next = ORSet(pairs + (e -> t))
    (next, Some(ORSet.Add(e, t)))
  }

  def contains(e: E): Boolean = pairs.exists(_._1 == e)

  def remove(e: E): ORSet.UpdateResult[E, T] = {
    val pairsToRemove = pairs.filter(_._1 == e)
    val next = ORSet(pairs -- pairsToRemove)
    (next, Some(ORSet.Remove(pairsToRemove)))
  }

  def run(operation: ORSet.Add[E, T]): ORSet.UpdateResult[E, T] =
    if (contains(operation.e)) {
      (this, None)
    } else {
      val next = ORSet(pairs + (operation.e -> operation.t))
      (next, Some(operation))
    }

  def run(operation: ORSet.Remove[E, T]): ORSet.UpdateResult[E, T] = (this, None)
}

object ORSet {
  type UpdateResult[E, T] = (ORSet[E, T], Option[ORSet.Update[E]])

  def apply[E, T: TombStone](): ORSet[E, T] = ORSet(Set.empty[(E, T)])

  sealed trait Update[E]
  case class Add[E, T: TombStone](e: E, t: T) extends Update[E]
  case class Remove[E, T: TombStone](pairs: Set[(E, T)]) extends Update[E]
}
