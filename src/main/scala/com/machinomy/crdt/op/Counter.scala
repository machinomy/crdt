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

package com.machinomy.crdt.op

case class Counter[E: Numeric](value: E)

object Counter {
  sealed trait Update[E]

  private def num[E: Numeric] = implicitly[Numeric[E]]

  case class Increment[E: Numeric](i: E) extends Update[E] {
    assert(num.gteq(i, num.zero))
  }

  case class Decrement[E: Numeric](i: E) extends Update[E] {
    assert(num.gteq(i, num.zero))
  }

  def apply[E: Numeric]() = new Counter[E](implicitly[Numeric[E]].zero)

  def update[E: Numeric](counter: Counter[E], update: Update[E]): Counter[E] = update match {
    case Counter.Increment(i) => Counter(num.plus(counter.value, i))
    case Counter.Decrement(i) => Counter(num.minus(counter.value, i))
  }
}
