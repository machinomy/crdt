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
import cats.syntax.all._

case class LWWElementSetA[E, T: TombStone : Ordering, B: Bias](additions: GTSet[E, T], removals: GTSet[E, T])
  extends Convergent[E, Set[E]]{

  type Self = LWWElementSetA[E, T, B]

  val tombStone = implicitly[TombStone[T]]

  def +(e: E) = copy(additions = additions + (e -> tombStone.next))

  def +(pair: (E, T)) = copy(additions = additions + pair)

  def -(e: E) = copy(removals = removals + (e -> tombStone.next))

  def -(pair: (E, T)) = copy(removals = removals + pair)

  override def value: Set[E] = {
    def fill(additions: Seq[(E, T)], removals: GTSet[E, T], result: Set[E] = Set.empty): Set[E] =
      if (additions.isEmpty) {
        result
      } else {
        val (element, additionT) = additions.head
        removals.state.get(element) match {
          case Some(removalT) =>
            val bias = implicitly[Bias[B]]
            bias.apply(element, additionT, removalT) match {
              case Some(e) =>
                fill(additions.tail, removals, result + element)
              case None =>
                fill(additions.tail, removals, result)
            }
          case None =>
            fill(additions.tail, removals, result + element)
        }
      }

    fill(additions.state.toSeq, removals)
  }
}

object LWWElementSetA {
  def apply[E, T: TombStone : Ordering, B: Bias](): LWWElementSetA[E, T, B] = new LWWElementSetA[E, T, B](GTSet[E, T](), GTSet[E, T]())

  implicit def semilattice[E, T: TombStone : Ordering, B: Bias] = new Semilattice[LWWElementSetA[E, T, B]] {
    override def combine(x: LWWElementSetA[E, T, B], y: LWWElementSetA[E, T, B]): LWWElementSetA[E, T, B] = {
      val additions = y.additions |+| x.additions
      val removals = y.removals |+| x.removals
      LWWElementSetA(additions, removals)
    }
  }
}
