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

/** Observed-Removed Set. An element is assigned sets of `additions` and `removals`.
  * Every addition results in adding a unique tag to the `additions` set. Removal leads to removing
  * the tags observed in a source replica for the element (see [[com.machinomy.crdt.state.ORSet.Moves]]). Actually,
  * it moves the tags to `removals` set. When combining, both sets are united for every element.
  *
  * @tparam E Element of the set
  * @tparam T Unique tag
  * @see [[com.machinomy.crdt.state.ORSet.monoid]] Behaves like a [[cats.Monoid]]
  * @see [[com.machinomy.crdt.state.ORSet.partialOrder]] Behaves like a [[cats.PartialOrder]]
  * @see Shapiro, M., Preguiça, N., Baquero, C., & Zawirski, M. (2011).
  *      Conflict-free replicated data types.
  *      In Proceedings of the 13th international conference on Stabilization, safety, and security of distributed systems (pp. 386–400).
  *      Grenoble, France: Springer-Verlag.
  *      Retrieved from [[http://dl.acm.org/citation.cfm?id=2050642]]
  */
case class ORSet[E, T: TombStone](state: Map[E, ORSet.Moves[T]]) extends Convergent[E, Set[E]] {
  type Self = ORSet[E, T]

  /** Add `element` to the set, using `stone` as a unique tag.
    *
    * @return Updated ORSet
    */
  def +(element: E, stone: T): Self = {
    val moves: ORSet.Moves[T] = state.getOrElse(element, ORSet.Moves[T]())
    val nextAdditions = moves.additions + stone
    val nextMoves = moves.copy(additions = nextAdditions)
    copy(state = state.updated(element, nextMoves))
  }

  /** Add `pair._1` to the set, using `pair._2` as a unique tag.
    *
    * @return Updated ORSet
    */
  def +(pair: (E, T)): Self = this + (pair._1, pair._2)

  /** Add `element` to the set, and generate `stone` on the fly.
    *
    * @return Updated ORSet
    */
  def +(element: E): Self = this + (element, implicitly[TombStone[T]].next)

  /** Remove `element` from the set.
    *
    * @return Updated ORSet
    */
  def -(element: E): Self = {
    val moves: ORSet.Moves[T] = state.getOrElse(element, ORSet.Moves[T]())
    val nextRemovals = moves.removals ++ moves.additions
    val nextMoves = moves.copy(removals = nextRemovals)
    copy(state = state.updated(element, nextMoves))
  }

  /** @return Value of the set.
    */
  override def value: Set[E] = state.keySet.filter { element =>
    val moves: ORSet.Moves[T] = state.getOrElse(element, ORSet.Moves[T]())
    (moves.additions -- moves.removals).nonEmpty
  }
}

object ORSet {

  /** Record addition and removal tags for [[ORSet]].
    *
    * @tparam T Addition and removal tag
    */
  case class Moves[T](additions: Set[T] = Set.empty[T], removals: Set[T] = Set.empty[T])

  def apply[E, T: TombStone](): ORSet[E, T] = ORSet[E, T](Map.empty[E, Moves[T]])

  /** Implements [[cats.Monoid]] type class for [[ORSet]].
    *
    * @tparam E Contained element
    * @tparam T Unique tag
    */
  implicit def monoid[E, T: TombStone] = new Monoid[ORSet[E, T]] {
    override def empty: ORSet[E, T] = new ORSet[E, T](Map.empty[E, Moves[T]])

    override def combine(x: ORSet[E, T], y: ORSet[E, T]): ORSet[E, T] = {
      val keys = x.state.keySet ++ y.state.keySet
      val nextStateSet =
        for {
          k <- keys
        } yield {
          val xMoves = x.state.getOrElse(k, Moves[T]())
          val yMoves = y.state.getOrElse(k, Moves[T]())
          val additions = xMoves.additions ++ yMoves.additions
          val removals = xMoves.removals ++ yMoves.removals
          k -> Moves(additions, removals)
        }
      new ORSet[E, T](nextStateSet.toMap)
    }
  }

  /** Implements [[cats.PartialOrder]] type class for [[ORSet]].
    *
    * @tparam E Contained element
    * @tparam T Unique tag
    */
  implicit def partialOrder[E, T: TombStone] = PartialOrder.byLteqv[ORSet[E, T]] { (x, y) =>
    val elements = x.state.keySet
    elements.forall { element =>
      val xMoves = x.state.getOrElse(element, Moves[T]())
      val yMoves = y.state.getOrElse(element, Moves[T]())
      val xAdditions = xMoves.additions
      val yAdditions = yMoves.additions
      val xRemovals = xMoves.removals
      val yRemovals = yMoves.removals
      (xAdditions subsetOf yAdditions) && (xRemovals subsetOf yRemovals)
    }
  }
}
