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

case class GSet[E](state: Set[E] = Set.empty[E]) extends Convergent[E, Set[E]] {
  type Self = GSet[E]

  def +(element: E) = new GSet[E](state + element)

  override def value: Set[E] = state
}

object GSet {
  implicit def semilattice[E] = new Semilattice[GSet[E]] {
    override def combine(x: GSet[E], y: GSet[E]): GSet[E] = {
      new GSet[E](x.value ++ y.value)
    }
  }
}
