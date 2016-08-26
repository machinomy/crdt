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

/**
  * 2P-Set, really, or two-phase value.
  *
  */
case class TPSet[E](additions: GSet[E] = GSet[E](), removals: GSet[E] = GSet[E]()) extends Convergent[E, Set[E]] {
  type Self = TPSet[E]

  def +(e: E): TPSet[E] = copy(additions = additions + e)

  def -(e: E): TPSet[E] = if (additions.value.contains(e)) {
    copy(removals = removals + e)
  } else {
    this
  }

  override def value: Set[E] = additions.value -- removals.value
}

object TPSet {
  implicit def semilattice[E] = new Semilattice[TPSet[E]] {
    override def combine(x: TPSet[E], y: TPSet[E]): TPSet[E] = {
      val additions = x.additions |+| y.additions
      val removals = x.removals |+| y.removals
      new TPSet[E](additions, removals)
    }
  }

  def apply[E](elements: Set[E]): TPSet[E] = TPSet[E](GSet[E](elements))
}
