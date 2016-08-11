package com.machinomy.crdt.state

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

  override def merge(that: Self): Self = {
    val keys = that.state.keySet ++ this.state.keySet
    val pairs =
      for (key <- keys) yield (this.state.get(key), that.state.get(key)) match {
        case (Some(a), Some(b)) =>
          key -> integral.max(a, b)
        case (Some(a), None) =>
          key -> a
        case (None, Some(b)) =>
          key -> b
        case (None, None) =>
          throw new IllegalArgumentException(s"Expected to retrieve value for key $key")
      }
    copy(state = pairs.toMap)
  }

  override def value: Set[E] = state.keySet.filter { element =>
    val change = state.getOrElse(element, integral.zero)
    isPresent(change)
  }

  /**
    * Odd means present.
    * @param tag
    * @return
    */
  def isPresent(tag: T): Boolean = integral.toInt(tag) % 2 != 0

  val integral = implicitly[Integral[T]]
}
