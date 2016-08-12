package com.machinomy.crdt.state

import cats.kernel.Semilattice

case class GSet[E](state: Set[E] = Set.empty[E]) extends Convergent[E, Set[E]] {
  type Self = GSet[E]

  def +(element: E) = new GSet[E](state + element)

  override def value: Set[E] = state
}

object GSet {
  implicit def semilattice[E] = new Semilattice[GSet[E]] {
    override def combine(x: GSet[E], y: GSet[E]): GSet[E] = {
      new GSet[E](x.value ++ y.value)
    }
  }
}
