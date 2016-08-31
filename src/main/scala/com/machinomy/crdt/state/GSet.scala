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

/** Grow-only set. `Combine` operation is a union of the sets.
  *
  * @tparam E Contained element
  * @see [[com.machinomy.crdt.state.GSet.monoid]] Behaves like a [[cats.Monoid]]
  * @see [[com.machinomy.crdt.state.GSet.partialOrder]] Behaves like a [[cats.PartialOrder]]
  * @see Shapiro, M., Preguiça, N., Baquero, C., & Zawirski, M. (2011).
  *      Conflict-free replicated data types.
  *      In Proceedings of the 13th international conference on Stabilization, safety, and security of distributed systems (pp. 386–400).
  *      Grenoble, France: Springer-Verlag.
  *      Retrieved from [[http://dl.acm.org/citation.cfm?id=2050642]]
  */
class GSet[E](val state: Set[E]) extends Convergent[E, Set[E]] {
  type Self = GSet[E]

  /** Add `element` to the set.
    *
    * @return Updated GSet
    */
  def +(element: E) = new GSet[E](state + element)

  /** @return Value of the set.
    */
  override def value: Set[E] = state
}

object GSet {
  /** Implements [[cats.Monoid]] type class for [[GSet]].
    *
    * @tparam E Contained element
    */
  implicit def monoid[E] = new Monoid[GSet[E]] {
    override def empty: GSet[E] = apply[E]()

    override def combine(x: GSet[E], y: GSet[E]): GSet[E] =
      new GSet[E](x.value ++ y.value)
  }

  /** Implements [[cats.PartialOrder]] type class for [[GSet]].
    *
    * @tparam E Contained element
    */
  implicit def partialOrder[E] = PartialOrder.byLteqv[GSet[E]] { (x, y) =>
    x.state subsetOf y.state
  }

  def apply[E]() = new GSet[E](Set.empty[E])

  def apply[E](state: Set[E]) = new GSet[E](state)
}
