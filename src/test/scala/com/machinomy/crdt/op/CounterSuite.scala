package com.machinomy.crdt.op

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

class CounterSuite extends FunSuite with PropertyChecks with Matchers {
  test("fresh value is zero") {
    val counter = Counter[Int]()
    assert(counter.value == 0)
  }

  test("increment") {
    forAll(Gen.posNum[Int]) { (i: Int) =>
      val increment = Counter.Increment(i)
      val counter = Counter.update(Counter[Int](), increment)
      counter.value should be(i)
    }
  }

  test("decrement") {
    forAll(Gen.posNum[Int]) { (i: Int) =>
      val decrement = Counter.Decrement(i)
      val counter = Counter.update(Counter[Int](), decrement)
      counter.value should be(-1 * i)
    }
  }
}
