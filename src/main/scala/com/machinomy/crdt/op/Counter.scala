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
