/*
 * Copyright 2016 Sergey Ukustov, Konstantin Makarychev
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

/** Grow-only counter. Could be incremented only.
  * The merge takes the maximum count for each replica.
  * Value is the sum of all replicas.
  *
  * @tparam R Replica identifier
  * @tparam E Counter element, must behave like [[scala.math.Numeric]]
  * @see [[com.machinomy.crdt.state.GCounter.semilattice]] Behaves like a [[cats.kernel.Semilattice]]
  * @example
  * {{{
  *   import com.machinomy.crdt.state.GCounter
  *   import cats.syntax.all._
  *
  *   val counter = GCounter[Int, Int]()
  *   val firstReplica = counter + (1 -> 1)
  *   val secondReplica = counter + (2 -> 2)
  *   val firstReplicaMerged = firstReplica |+| secondReplica
  *   val secondReplicaMerged = secondReplica |+| firstReplica
  *
  *   firstReplicaMerged == secondReplicaMerged
  * }}}
  */
case class GCounter[R, E](state: Map[R, E] = Map.empty[R, E])(implicit num: Numeric[E]) extends Convergent[E, E] {
  type Self = GCounter[R, E]

  /** Increment value for replica `pair._1` by `pair._2`. Only positive values are allowed.
    *
    * @see [[increment]]
    * @param pair Replica identifier
    * @return New GCounter state
    */
  def +(pair: (R, E)): Self = increment(pair._1, pair._2)

  /** Increment value for replica `replicaId` by `delta`. Only positive values are allowed.
    *
    * @see [[+]]
    * @param replicaId Replica identifier.
    * @param delta     Increment of a counter
    * @return New GCounter state
    */
  def increment(replicaId: R, delta: E): Self = {
    require(num.gteq(delta, num.zero), "Can only increment GCounter")
    if (num.equiv(delta, num.zero)) {
      this
    } else {
      state.get(replicaId) match {
        case Some(value) => new GCounter[R, E](state.updated(replicaId, num.plus(value, delta)))
        case None => new GCounter[R, E](state.updated(replicaId, delta))
      }
    }
  }

  /** Check if empty.
    *
    * @return True if no replicas.
    */
  def isEmpty: Boolean = state.isEmpty

  /** Value for `replicaId`, or zero if absent.
    *
    * @param replicaId Replica identifier
    * @return
    */
  def get(replicaId: R): E = state.getOrElse(replicaId, num.zero)

  /** @return Value of the counter.
    */
  override def value: E = state.values.sum
}

object GCounter {
  /** Implements [[cats.kernel.Semilattice]] type class for [[GCounter]].
    *
    * @tparam R Replica identifier
    * @tparam E Counter element, must behave like [[scala.math.Numeric]]
    */
  implicit def semilattice[R, E](implicit num: Numeric[E]) = new Semilattice[GCounter[R, E]] {
    override def combine(x: GCounter[R, E], y: GCounter[R, E]): GCounter[R, E] = {
      def fill(ids: Set[R], a: Map[R, E], b: Map[R, E], result: Map[R, E] = Map.empty): Map[R, E] =
        if (ids.isEmpty) {
          result
        } else {
          val key = ids.head
          val valueA = a.getOrElse(key, num.zero)
          val valueB = b.getOrElse(key, num.zero)
          fill(ids.tail, a, b, result.updated(key, num.max(valueA, valueB)))
        }
      val ids = x.state.keySet ++ y.state.keySet
      GCounter(fill(ids, x.state, y.state))
    }
  }
}
