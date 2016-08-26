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

case class LWWRegister[E, T: TombStone : Ordering](value: E, timestamp: T) extends Convergent[E, E]

object LWWRegister {
  def apply[E, T: TombStone : Ordering](value: E): LWWRegister[E, T] = LWWRegister[E, T](value, implicitly[TombStone[T]].next)

  implicit def semilattice[E, T: TombStone : Ordering] = new Semilattice[LWWRegister[E, T]] {
    override def combine(x: LWWRegister[E, T], y: LWWRegister[E, T]): LWWRegister[E, T] = {
      val ordering = implicitly[Ordering[T]]
      if (ordering.gt(x.timestamp, y.timestamp)) {
        x
      } else {
        y
      }
    }
  }
}
