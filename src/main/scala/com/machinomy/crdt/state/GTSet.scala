package com.machinomy.crdt.state

/**
  * G-Set that tracks time of addition.
  *
  * @param state
  * @tparam E
  * @tparam T
  */
case class GTSet[E, T: TombStone](state: Map[E, T] = Map.empty[E, T]) extends Convergent[E, Set[E]] {

  override type Self = GTSet[E, T]

  val tombStone = implicitly[TombStone[T]]

  def +(e: E): Self = new GTSet[E, T](state + (e -> tombStone.next))

  def +(tuple: (E, T)): Self = new GTSet[E, T](state + tuple)

  override def merge(that: Self): Self = {
    def fill(keys: Set[E], as: Map[E, T], bs: Map[E, T], result: Map[E, T] = Map.empty[E, T]): Map[E, T] =
      if (keys.isEmpty) {
        result
      } else {
        val key = keys.head
        val value = (as.get(key), bs.get(key)) match {
          case (Some(a), Some(b)) => tombStone.ordering.max(a, b)
          case (Some(a), None) => a
          case (None, Some(b)) => b
          case (None, None) => throw new IllegalArgumentException(s"Expected to retrieve value for key $key")
        }
        fill(keys.tail, as, bs, result.updated(key, value))
      }

    val keys: Set[E] = state.keySet ++ that.state.keySet
    val nextState = fill(keys, this.state, that.state)
    new GTSet[E, T](nextState)
  }

  override def value: Set[E] = state.keySet
}
