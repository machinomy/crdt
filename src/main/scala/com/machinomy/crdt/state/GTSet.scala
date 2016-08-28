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

/**
  * G-Set that tracks time of addition. `Combine` operation takes the value added last.
  *
  * @tparam E Contained element
  * @tparam T Time
  * @see [[com.machinomy.crdt.state.GTSet.monoid]] Behaves like [[cats.Monoid]]
  * @see [[com.machinomy.crdt.state.GTSet.partialOrder]] Behaves like [[cats.PartialOrder]]
  */
case class GTSet[E, T](state: Map[E, T] = Map.empty[E, T])(implicit tombStone: TombStone[T]) extends Convergent[E, Set[E]] {
  type Self = GTSet[E, T]

  /** Add `element` to the set using the current TombStone.
    *
    * @return Updated GTSet.
    */
  def +(element: E): Self = new GTSet[E, T](state + (element -> tombStone.next))

  /** Add element `pair._1` with provided tomb stone `pair._2`.
    *
    * @return Updated GTSet.
    */
  def +(pair: (E, T)): Self = new GTSet[E, T](state + pair)

  /** @return Value of the set.
    */
  override def value: Set[E] = state.keySet
}

object GTSet {
  /** Implements [[cats.Monoid]] type class for [[GTSet]].
    *
    * @tparam E Contained element
    */
  implicit def monoid[E, T: TombStone](implicit ordering: Ordering[T]) = new Monoid[GTSet[E, T]] {
    override def empty: GTSet[E, T] = new GTSet[E, T](Map.empty[E, T])

    override def combine(x: GTSet[E, T], y: GTSet[E, T]): GTSet[E, T] = {
      def fill(keys: Set[E], as: Map[E, T], bs: Map[E, T], result: Map[E, T] = Map.empty[E, T]): Map[E, T] =
        if (keys.isEmpty) {
          result
        } else {
          val key = keys.head
          val value = (as.get(key), bs.get(key)) match {
            case (Some(a), Some(b)) => ordering.max(a, b)
            case (Some(a), None) => a
            case (None, Some(b)) => b
            case (None, None) => throw new IllegalArgumentException(s"Expected to retrieve value for key $key")
          }
          fill(keys.tail, as, bs, result.updated(key, value))
        }
      val keys: Set[E] = x.state.keySet ++ y.state.keySet
      val nextState = fill(keys, x.state, y.state)
      new GTSet[E, T](nextState)
    }
  }

  /** Implements [[cats.PartialOrder]] type class for [[GTSet]].
    *
    *  @tparam E Contained element
    */
  implicit def partialOrder[E] = PartialOrder.byLteqv[GSet[E]] { (x, y) =>
    x.state subsetOf y.state
  }
}
