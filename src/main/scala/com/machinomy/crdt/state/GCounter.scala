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

case class GCounter[K, E : Numeric](state: Map[K, E] = Map.empty[K, E]) extends Convergent[E, E] {
  type Self = GCounter[K, E]

  def +(i: (K, E)): Self = increment(i._1, i._2)

  def increment(key: K, delta: E): Self = {
    val num = implicitly[Numeric[E]]
    require(num.gteq(delta, num.zero), "Can only increment GCounter")

    if (num.equiv(delta, num.zero)) {
      this
    } else {
      state.get(key) match {
        case Some(value) => new GCounter[K, E](state.updated(key, num.plus(value, delta)))
        case None => new GCounter[K, E](state.updated(key, delta))
      }
    }
  }

  def isEmpty = state.isEmpty

  def get(key: K): E = {
    val num = implicitly[Numeric[E]]
    state.getOrElse(key, num.zero)
  }

  override def value: E = state.values.sum

  val num = implicitly[Numeric[E]]
}

object GCounter {
  implicit def semilattice[K, E: Numeric] = new Semilattice[GCounter[K, E]] {
    val num = implicitly[Numeric[E]]
    override def combine(x: GCounter[K, E], y: GCounter[K, E]): GCounter[K, E] = {
      val keys: Set[K] = x.state.keySet ++ y.state.keySet
      def fill(keys: Set[K], a: Map[K, E], b: Map[K, E], result: Map[K, E] = Map.empty): Map[K, E] =
        if (keys.isEmpty) {
          result
        } else {
          val key = keys.head
          val valueA = a.getOrElse(key, num.zero)
          val valueB = b.getOrElse(key, num.zero)
          fill(keys.tail, a, b, result.updated(key, num.max(valueA, valueB)))
        }
      GCounter(fill(keys, x.state, y.state))
    }
  }
}
