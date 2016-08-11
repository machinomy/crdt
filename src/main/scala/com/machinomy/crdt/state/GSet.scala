package com.machinomy.crdt.state

case class GSet[E](state: Set[E] = Set.empty[E]) extends Convergent[E, Set[E]] {
  override type Self = GSet[E]

  def +(element: E) = new GSet[E](state + element)

  override def merge(that: Self): Self = new GSet[E](value ++ that.value)

  override def value: Set[E] = state
}
