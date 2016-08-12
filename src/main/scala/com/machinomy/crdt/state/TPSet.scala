package com.machinomy.crdt.state

import cats.kernel.Semilattice
import cats.syntax.all._

/**
  * 2P-Set, really, or two-phase state.
  *
  */
case class TPSet[E](additions: GSet[E] = GSet[E](), removals: GSet[E] = GSet[E]()) extends Convergent[E, Set[E]] {
  override type Self = TPSet[E]

  def +(e: E): TPSet[E] = copy(additions = additions + e)

  def -(e: E): TPSet[E] = if (additions.value.contains(e)) {
    copy(removals = removals + e)
  } else {
    this
  }

  override def value: Set[E] = additions.value -- removals.value
}

object TPSet {
  implicit def semilattice[E] = new Semilattice[TPSet[E]] {
    override def combine(x: TPSet[E], y: TPSet[E]): TPSet[E] = {
      val additions = x.additions |+| y.additions
      val removals = x.removals |+| y.removals
      new TPSet[E](additions, removals)
    }
  }
}
