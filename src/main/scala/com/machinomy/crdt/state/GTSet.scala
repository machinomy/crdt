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

/**
  * G-Set that tracks time of addition.
  *
  */
case class GTSet[E, T: TombStone : Ordering](state: Map[E, T] = Map.empty[E, T]) extends Convergent[E, Set[E]] {
  type Self = GTSet[E, T]

  val tombStone = implicitly[TombStone[T]]
  val ordering = implicitly[Ordering[T]]

  def +(e: E): Self = new GTSet[E, T](state + (e -> tombStone.next))

  def +(tuple: (E, T)): Self = new GTSet[E, T](state + tuple)

  override def value: Set[E] = state.keySet
}

object GTSet {
  implicit def semilattice[E, T: TombStone : Ordering] = new Semilattice[GTSet[E, T]] {
    val ordering = implicitly[Ordering[T]]

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
}
