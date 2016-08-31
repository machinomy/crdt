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

/** Max-Change Set. Assigns each element an integer. If the number is odd, the element is considered present.
  * Odd number means the element is absent. To add an element you increment __odd__ number. To remove the element,
  * __even__ number is decremented.
  *
  * @tparam E Element contained.
  * @tparam T Number assigned to an element, must implement [[Integral]] type class.
  * @todo Find a paper to cite.
  */
case class MCSet[E, T](state: Map[E, T])(implicit integral: Integral[T]) extends Convergent[E, Set[E]] {
  type Self = MCSet[E, T]

  /** Add an element to the set.
    *
    * @return Updated MCSet.
    */
  def +(element: E): Self = {
    val tag = state.getOrElse(element, integral.zero)
    if (isPresent(tag)) {
      this
    } else {
      increment(element, tag)
    }
  }

  /** Remove an element from the set.
    *
    * @return Updated MCSet.
    */
  def -(element: E): Self = {
    val tag = state.getOrElse(element, integral.zero)
    if (isPresent(tag)) {
      increment(element, tag)
    } else {
      this
    }
  }

  /** Contained [[Set]].
    *
    * @return
    */
  override def value: Set[E] = state.keySet.filter { element =>
    val change = state.getOrElse(element, integral.zero)
    isPresent(change)
  }

  /** Odd means present
    *
    * @param tag Integer assigned for the element
    * @return
    */
  protected def isPresent(tag: T): Boolean = integral.toInt(tag) % 2 != 0

  private def increment(element: E, tag: T): Self = {
    val nextTag = integral.plus(tag, integral.one)
    val nextState = state.updated(element, nextTag)
    copy(state = nextState)
  }
}

object MCSet {
  /** Implements [[cats.Monoid]] type class for [[MCSet]].
    *
    * @tparam E Contained element
    */
  implicit def monoid[E, T](implicit integral: Integral[T]) = new Monoid[MCSet[E, T]] {
    override def empty: MCSet[E, T] = new MCSet[E, T](Map.empty[E, T])

    override def combine(x: MCSet[E, T], y: MCSet[E, T]): MCSet[E, T] = {
      val keys = x.state.keySet ++ y.state.keySet
      val pairs =
        for (key <- keys) yield (x.state.get(key), y.state.get(key)) match {
          case (Some(a), Some(b)) =>
            key -> integral.max(a, b)
          case (Some(a), None) =>
            key -> a
          case (None, Some(b)) =>
            key -> b
          case (None, None) =>
            throw new IllegalArgumentException(s"Expected to retrieve value for key $key")
        }
      MCSet(pairs.toMap)
    }
  }

  /** Implements [[cats.PartialOrder]] type class for [[MCSet]].
    *
    *  @tparam E Contained element
    */
  implicit def partialOrder[E, T](implicit integral: Integral[T]) = PartialOrder.byLteqv[MCSet[E, T]] { (x, y) =>
    val ids = x.state.keySet
    ids.forall { id =>
      val yTag = y.state.getOrElse(id, integral.zero)
      val xTag = x.state.getOrElse(id, integral.zero)
      integral.lteq(xTag, yTag)
    }
  }
}
