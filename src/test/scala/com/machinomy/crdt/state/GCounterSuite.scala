package com.machinomy.crdt.state

import org.scalacheck.Gen
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks

class GCounterSuite extends FunSuite with PropertyChecks with Matchers {
  val replicaGen = Gen.posNum[Int]
  val valueGen = Gen.posNum[Int]
  val counterGen = for {
    id <- replicaGen
    value <- valueGen
  } yield GCounter[Int, Int]() + (id -> value)

  test("Fresh GCounter is empty") {
    val fresh = GCounter[Int, Int]()
    assert(fresh.isEmpty)
    assert(fresh.value === 0)
  }

  test("could be calculated") {
    val counter = GCounter[Int, Int]().increment(1, 2).increment(2, 3)
    assert(counter.value === 5)
  }

  test("could be merged") {
    val a = GCounter[Int, Int]() + (1, 2) + (2, 3)
    val b = GCounter[Int, Int]() + (1, 2) + (3, 4)
    val c = a.merge(b)
    assert(c.value === 2 + 3 + 4)
  }

  test("could present replica value") {
    val a = GCounter[Int, Int]()
    assert(a.get(0) === 0)
    val b = a.increment(1, 2).increment(2, 3)
    assert(b.get(1) === 2)
    assert(b.get(2) === 3)
  }

  test("equality") {
    val a = GCounter[Int, Int]()
    val b = GCounter[Int, Int]()
    assert(a === b)

    val a1 = a + (1 -> 1)
    assert(a1 !== b)
    val b1 = b + (1 -> 2)
    assert(a1 !== b1)
    val a2 = a1 + (1 -> 1)
    assert(a2 === b1)
  }

  test("associativity") {
    forAll(Gen.listOfN(3, counterGen)) { case a :: b :: c :: Nil =>
      (a merge (b merge c)) should be((a merge b) merge c)
    }
  }

  test("commutativity") {
    forAll(Gen.listOfN(2, counterGen)) { case a :: b :: Nil =>
      (a merge b) should be(b merge a)
    }
  }

  test("idempotency") {
    forAll(Gen.listOf(counterGen)) { list =>
      whenever(list.nonEmpty) {
        val counter = list.reduce(_ merge _)
        (counter merge counter) should be(counter)
      }
    }
  }
}
