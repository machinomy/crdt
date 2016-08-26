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

import cats.kernel.Semilattice

case class ORSet[E, T: TombStone](state: Map[E, ORSet.Moves[T]]) extends Convergent[E, Set[E]] {
  type Self = ORSet[E, T]

  def +(element: E, tomb: T): Self = {
    val moves: ORSet.Moves[T] = state.getOrElse(element, ORSet.Moves[T]())
    val nextAdditions = moves.additions + tomb
    val nextMoves = moves.copy(additions = nextAdditions)
    copy(state = state.updated(element, nextMoves))
  }

  def +(pair: (E, T)): Self = this + (pair._1, pair._2)

  def +(element: E): Self = this + (element, implicitly[TombStone[T]].next)

  def -(element: E): Self = {
    val moves: ORSet.Moves[T] = state.getOrElse(element, ORSet.Moves[T]())
    val nextRemovals = moves.removals ++ moves.additions
    val nextMoves = moves.copy(removals = nextRemovals)
    copy(state = state.updated(element, nextMoves))
  }

  override def value: Set[E] = state.keySet.filter { element =>
    val moves: ORSet.Moves[T] = state.getOrElse(element, ORSet.Moves[T]())
    (moves.additions -- moves.removals).nonEmpty
  }
}

object ORSet {
  case class Moves[T](additions: Set[T] = Set.empty[T], removals: Set[T] = Set.empty[T])

  def apply[E, T: TombStone](): ORSet[E, T] = ORSet[E, T](Map.empty[E, Moves[T]])

  implicit def semilattice[E, T: TombStone] = new Semilattice[ORSet[E, T]] {
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
}
