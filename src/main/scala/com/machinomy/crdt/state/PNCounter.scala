package com.machinomy.crdt.state

case class PNCounter[K, E: Numeric](increments: GCounter[K, E], decrements: GCounter[K, E]) extends Convergent[E, E] {
  override type Self = PNCounter[K, E]

  def +(i: (K, E)): Self = {
    val delta = i._2
    if (num.gteq(delta, num.zero)) {
      copy(increments = increments + i)
    } else {
      val positive = (i._1, num.minus(num.zero, i._2))
      copy(decrements = decrements + positive)
    }
  }

  def -(i: (K, E)): Self = this + i

  def get(k: K): E = num.minus(increments.get(k), decrements.get(k))

  def table: Map[K, E] = {
    def fill(keys: Set[K], incs: Map[K, E], decs: Map[K, E], table: Map[K, E] = Map.empty): Map[K, E] =
      if (keys.isEmpty) {
        table
      } else {
        val key = keys.head
        val inc = incs.getOrElse(key, num.zero)
        val dec = decs.getOrElse(key, num.zero)
        fill(keys.tail, incs, decs, table.updated(key, num.minus(inc, dec)))
      }

    val keys: Set[K] = increments.state.keySet ++ decrements.state.keySet
    fill(keys, increments.state, decrements.state)
  }

  val num: Numeric[E] = implicitly[Numeric[E]]

  override def merge(other: PNCounter[K, E]): PNCounter[K, E] = {
    new PNCounter[K, E](other.increments.merge(this.increments), other.decrements.merge(this.decrements))
  }

  override def value: E = num.minus(increments.value, decrements.value)
}

object PNCounter {
  def apply[K, E: Numeric](): PNCounter[K, E] = new PNCounter(GCounter[K, E](), GCounter[K, E]())
}
