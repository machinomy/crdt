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

import cats.Order

trait Bias[B] {
  def apply[E, T: TombStone : Order](element: E, addition: T, removal: T): Option[E]
}

object Bias {
  sealed trait Direction
  case class AdditionWins() extends Direction
  case class RemovalWins() extends Direction

  implicit val additionBias = new Bias[AdditionWins] {
    override def apply[E, T: TombStone : Order](element: E, add: T, remove: T): Option[E] = {
      val Order = implicitly[Order[T]]
      if (Order.gteqv(add, remove)) {
        Some(element)
      } else {
        None
      }
    }
  }

  implicit val removalBias = new Bias[RemovalWins] {
    override def apply[E, T: TombStone : Order](element: E, add: T, remove: T): Option[E] = {
      val Order = implicitly[Order[T]]
      if (Order.gteqv(remove, add)) {
        None
      } else {
        Some(element)
      }
    }
  }
}
