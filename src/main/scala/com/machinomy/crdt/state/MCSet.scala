package com.machinomy.crdt.state

import cats.kernel.Semilattice

case class MCSet[E, T: Integral](state: Map[E, T] = Map.empty[E, T]) extends Convergent[E, Set[E]] {
  override type Self = MCSet[E, T]

  def +(element: E): Self = {
    val tag = state.getOrElse(element, integral.zero)
    if (isPresent(tag)) {
      this
    } else {
      increment(element, tag)
    }
  }

  def -(element: E): Self = {
    val tag = state.getOrElse(element, integral.zero)
    if (isPresent(tag)) {
      increment(element, tag)
    } else {
      this
    }
  }

  private def increment(element: E, tag: T): Self = {
    val nextTag = integral.plus(tag, integral.one)
    val nextState = state.updated(element, nextTag)
    copy(state = nextState)
  }

  override def value: Set[E] = state.keySet.filter { element =>
    val change = state.getOrElse(element, integral.zero)
    isPresent(change)
  }

  /**
    * Odd means present.
    */
  def isPresent(tag: T): Boolean = integral.toInt(tag) % 2 != 0

  val integral = implicitly[Integral[T]]
}

object MCSet {
  implicit def semilattice[E, T: Integral] = new Semilattice[MCSet[E, T]] {
    val integral = implicitly[Integral[T]]
    override def combine(x: MCSet[E, T], y: MCSet[E, T]): MCSet[E, T] = {
      val keys = x.state.keySet ++ y.state.keySet
      val pairs =
        for (key <- keys) yield (x.state.get(key), y.state.get(key)) match {
          case (Some(a), Some(b)) =>
            key -> integral.max(a, b)
          case (Some(a), None) =>
            key -> a
          case (None, Some(b)) =>
            key -> b
          case (None, None) =>
            throw new IllegalArgumentException(s"Expected to retrieve value for key $key")
        }
      MCSet(pairs.toMap)
    }
  }
}
