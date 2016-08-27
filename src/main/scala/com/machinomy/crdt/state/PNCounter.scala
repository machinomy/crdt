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

/** Positive-Negative counter. Allows increments and decrements both.
  * `combine` operation takes the maximum count for each replica, for each of increments and decrements sets.
  * Value is the sum of all replicas.
  *
  * @tparam R Replica identifier
  * @tparam E Counter element, must behave like [[scala.math.Numeric]]
  * @see [[com.machinomy.crdt.state.PNCounter.monoid]] Behaves like a [[cats.Monoid]]
  * @see [[com.machinomy.crdt.state.PNCounter.partialOrder]] Behaves like a [[cats.PartialOrder]]
  * @example
  * {{{
  *   import com.machinomy.crdt.state._
  *   import cats.syntax.all._
  *   import cats._
  *
  *   val counter = Monoid[PNCounter[Int, Int]].empty // fresh PN-Counter
  *   val firstReplica = counter + (1 -> 1) // increment replica 1
  *   val secondReplica = counter + (2 -> -2) // decrement replica 2
  *   val firstReplicacombined = firstReplica |+| secondReplica // combine
  *   val secondReplicacombined = secondReplica |+| firstReplica // combine
  *   firstReplicacombined == secondReplicacombined // the result is independent of combine order
  *   firstReplicacombined.value == -1
  * }}}
  */
case class PNCounter[R, E](increments: GCounter[R, E], decrements: GCounter[R, E])(implicit num: Numeric[E]) extends Convergent[E, E] {
  type Self = PNCounter[R, E]

  /** Increment or decrement value for replica `pair._1` by `pair._2`.
    *
    * @param pair Replica identifier
    * @return Updated PNCounter
    */
  def +(pair: (R, E)): Self = {
    val delta = pair._2
    if (num.gteq(delta, num.zero)) {
      copy(increments = increments + pair)
    } else {
      val positive = (pair._1, num.minus(num.zero, pair._2))
      copy(decrements = decrements + positive)
    }
  }

  /** Value for `replicaId`, or zero if absent.
    *
    * @param replicaId Replica identifier
    * @return
    */
  def get(replicaId: R): E = num.minus(increments.get(replicaId), decrements.get(replicaId))

  /** Values per replica, increments minus decrements.
    */
  def table: Map[R, E] = {
    def fill(keys: Set[R], incs: Map[R, E], decs: Map[R, E], table: Map[R, E] = Map.empty): Map[R, E] =
      if (keys.isEmpty) {
        table
      } else {
        val key = keys.head
        val inc = incs.getOrElse(key, num.zero)
        val dec = decs.getOrElse(key, num.zero)
        fill(keys.tail, incs, decs, table.updated(key, num.minus(inc, dec)))
      }

    val keys: Set[R] = increments.state.keySet ++ decrements.state.keySet
    fill(keys, increments.state, decrements.state)
  }

  /** @return Value of the counter.
    */
  override def value: E = num.minus(increments.value, decrements.value)
}

object PNCounter {
  /** Implements [[cats.Monoid]] type class for [[PNCounter]].
    *
    * @tparam R Replica identifier
    * @tparam E Counter element, must behave like [[scala.math.Numeric]]
    */
  implicit def monoid[R, E: Numeric] = new Monoid[PNCounter[R, E]] {
    override def empty: PNCounter[R, E] = new PNCounter(Monoid[GCounter[R, E]].empty, Monoid[GCounter[R, E]].empty)

    override def combine(x: PNCounter[R, E], y: PNCounter[R, E]): PNCounter[R, E] = {
      val increments = x.increments |+| y.increments
      val decrements = x.decrements |+| y.decrements
      new PNCounter[R, E](increments, decrements)
    }
  }

  /** Implements [[cats.PartialOrder]] type class for [[PNCounter]].
    *
    * @tparam R Replica identifier
    * @tparam E Counter element, must behave like [[scala.math.Numeric]]
    */
  implicit def partialOrder[R, E](implicit num: Numeric[E]) = PartialOrder.byLteqv[PNCounter[R, E]] { (x, y) =>
    val ids = x.increments.state.keySet ++
      y.increments.state.keySet ++
      x.decrements.state.keySet ++
      y.decrements.state.keySet
    ids.forall { id =>
      val xInc = x.increments.state.getOrElse(id, num.zero)
      val yInc = y.increments.state.getOrElse(id, num.zero)
      val xDec = x.decrements.state.getOrElse(id, num.zero)
      val yDec = y.decrements.state.getOrElse(id, num.zero)
      num.lteq(xInc, yInc) && num.lteq(xDec, yDec)
    }
  }
}
