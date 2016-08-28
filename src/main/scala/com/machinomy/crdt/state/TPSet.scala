/*
 * Copyright 2016 Machinomy
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.machinomy.crdt.state

import cats._
import cats.syntax.all._

/**
  * 2P-Set, or two-phase set. Contains one [[GSet]] for additions, and one for removals
  * Removing of an element is allowed, only if it is present in the set of additions.
  * `Combine` operation combines additions and removals as a GSet.
  *
  * @tparam E Contained element
  * @see [[com.machinomy.crdt.state.TPSet.monoid]] Behaves like a [[cats.Monoid]]
  * @see [[com.machinomy.crdt.state.TPSet.partialOrder]] Behaves like a [[cats.PartialOrder]]
  * @see Shapiro, M., Preguiça, N., Baquero, C., & Zawirski, M. (2011).
  *      Conflict-free replicated data types.
  *      In Proceedings of the 13th international conference on Stabilization, safety, and security of distributed systems (pp. 386–400).
  *      Grenoble, France: Springer-Verlag.
  *      Retrieved from [[http://dl.acm.org/citation.cfm?id=2050642]]
  */
case class TPSet[E](additions: GSet[E] = GSet[E](), removals: GSet[E] = GSet[E]()) extends Convergent[E, Set[E]] {
  type Self = TPSet[E]

  /** Add element to the set.
    *
    * @return Updated TPSet.
    */
  def +(element: E): TPSet[E] = copy(additions = additions + element)

  /** Remove element from the set.
    *
    * @return Updated TPSet.
    */
  def -(element: E): TPSet[E] = if (additions.value.contains(element)) {
    copy(removals = removals + element)
  } else {
    this
  }

  /** @return Value of the set.
    */
  override def value: Set[E] = additions.value -- removals.value
}

object TPSet {
  /** Implements [[cats.Monoid]] type class for [[TPSet]].
    *
    * @tparam E Contained element
    */
  implicit def monoid[E](implicit gSetMonoid: Monoid[GSet[E]]) = new Monoid[TPSet[E]] {
    override def empty: TPSet[E] = new TPSet[E](gSetMonoid.empty, gSetMonoid.empty)

    override def combine(x: TPSet[E], y: TPSet[E]): TPSet[E] = {
      val additions = x.additions |+| y.additions
      val removals = x.removals |+| y.removals
      new TPSet[E](additions, removals)
    }
  }

  /** Implements [[cats.PartialOrder]] type class for [[TPSet]].
    *
    *  @tparam E Contained element
    */
  implicit def partialOrder[E](implicit gSetPartialOrder: PartialOrder[GSet[E]]) = PartialOrder.byLteqv[TPSet[E]] { (x, y) =>
    val additions = gSetPartialOrder.lteqv(x.additions, y.additions)
    val removals = gSetPartialOrder.lteqv(x.removals, y.removals)
    additions && removals
  }

  def apply[E](elements: Set[E]): TPSet[E] = TPSet[E](GSet[E](elements))
}
