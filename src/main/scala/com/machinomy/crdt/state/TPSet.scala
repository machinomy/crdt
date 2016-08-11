package com.machinomy.crdt.state

/**
  * 2P-Set, really, or two-phase state.
  *
  * @param additions
  * @param removals
  * @tparam E
  */
case class TPSet[E](additions: GSet[E] = GSet[E](), removals: GSet[E] = GSet[E]()) extends Convergent[E, Set[E]] {
  override type Self = TPSet[E]

  def +(e: E): TPSet[E] = copy(additions = additions + e)

  def -(e: E): TPSet[E] = if (additions.value.contains(e)) {
    copy(removals = removals + e)
  } else {
    this
  }

  override def merge(other: TPSet[E]): TPSet[E] = {
    val nextAdditions = other.additions.merge(additions)
    val nextRemovals = other.removals.merge(removals)
    new TPSet[E](nextAdditions, nextRemovals)
  }

  override def value: Set[E] = additions.value -- removals.value
}
