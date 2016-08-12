package com.machinomy.crdt.state

import cats.kernel.Semilattice

/**
  * G-Set that tracks time of addition.
  *
  */
case class GTSet[E, T: TombStone : Ordering](state: Map[E, T] = Map.empty[E, T]) extends Convergent[E, Set[E]] {

  override type Self = GTSet[E, T]

  val tombStone = implicitly[TombStone[T]]
  val ordering = implicitly[Ordering[T]]

  def +(e: E): Self = new GTSet[E, T](state + (e -> tombStone.next))

  def +(tuple: (E, T)): Self = new GTSet[E, T](state + tuple)

  override def value: Set[E] = state.keySet
}

object GTSet {
  implicit def semilattice[E, T: TombStone : Ordering] = new Semilattice[GTSet[E, T]] {
    val ordering = implicitly[Ordering[T]]

    override def combine(x: GTSet[E, T], y: GTSet[E, T]): GTSet[E, T] = {
      def fill(keys: Set[E], as: Map[E, T], bs: Map[E, T], result: Map[E, T] = Map.empty[E, T]): Map[E, T] =
        if (keys.isEmpty) {
          result
        } else {
          val key = keys.head
          val value = (as.get(key), bs.get(key)) match {
            case (Some(a), Some(b)) => ordering.max(a, b)
            case (Some(a), None) => a
            case (None, Some(b)) => b
            case (None, None) => throw new IllegalArgumentException(s"Expected to retrieve value for key $key")
          }
          fill(keys.tail, as, bs, result.updated(key, value))
        }
      val keys: Set[E] = x.state.keySet ++ y.state.keySet
      val nextState = fill(keys, x.state, y.state)
      new GTSet[E, T](nextState)
    }
  }
}
