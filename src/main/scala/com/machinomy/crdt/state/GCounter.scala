package com.machinomy.crdt.state

case class GCounter[K, E : Numeric](state: Map[K, E] = Map.empty[K, E]) extends Convergent[E, E] {
  override type Self = GCounter[K, E]

  def +(i: (K, E)): Self = increment(i._1, i._2)

  def increment(key: K, delta: E): Self = {
    val num = implicitly[Numeric[E]]
    require(num.gteq(delta, num.zero), "Can only increment GCounter")

    if (num.equiv(delta, num.zero)) {
      this
    } else {
      state.get(key) match {
        case Some(value) => new GCounter[K, E](state.updated(key, num.plus(value, delta)))
        case None => new GCounter[K, E](state.updated(key, delta))
      }
    }
  }

  def isEmpty = state.isEmpty

  def get(key: K): E = {
    val num = implicitly[Numeric[E]]
    state.getOrElse(key, num.zero)
  }

  override def merge(other: Self): Self = {
    val keys: Set[K] = state.keySet ++ other.state.keySet
    def fill(keys: Set[K], a: Map[K, E], b: Map[K, E], result: Map[K, E] = Map.empty): Map[K, E] =
      if (keys.isEmpty) {
        result
      } else {
        val key = keys.head
        val valueA = a.getOrElse(key, num.zero)
        val valueB = b.getOrElse(key, num.zero)
        fill(keys.tail, a, b, result.updated(key, num.max(valueA, valueB)))
      }
    GCounter(fill(keys, state, other.state))
  }

  override def value: E = state.values.sum

  val num = implicitly[Numeric[E]]
}