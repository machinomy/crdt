package com.machinomy.crdt.state

import cats.kernel.{Eq, Monoid}
import cats.syntax.all._
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class GCounterSuite extends FunSuite with PropertyChecks {
  val replicaGen = Gen.posNum[Int]
  val valueGen = Gen.posNum[Int]
  val counterGen = for {
    id <- replicaGen
    value <- valueGen
  } yield GCounter[Int, Int]() + (id -> value)
  val eq = implicitly[Eq[GCounter[Int, Int]]]

  test("Fresh GCounter is empty") {
    val fresh = GCounter[Int, Int]()
    assert(Monoid[GCounter[Int, Int]].isEmpty(fresh))
    assert(fresh.value === 0)
  }

  test("could be calculated") {
    val counter = GCounter[Int, Int]().increment(1, 2).increment(2, 3)
    assert(counter.value === 5)
  }

  test("could be combined") {
    val a = GCounter[Int, Int]() + (1, 2) + (2, 3)
    val b = GCounter[Int, Int]() + (1, 2) + (3, 4)
    val c = a |+| b
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
    assert(eq.eqv(a, b))

    val a1 = a + (1 -> 1)
    assert(eq.neqv(a1, b))
    val b1 = b + (1 -> 2)
    assert(a1 !== b1)
    assert(eq.neqv(a1, b1))
    val a2 = a1 + (1 -> 1)
    assert(eq.eqv(a2, b1))
  }

  test("associativity") {
    forAll(Gen.listOfN(3, counterGen)) {
      case x :: y :: z :: Nil =>
        val left = x |+| (y |+| z)
        val right = (x |+| y) |+| z
        assert(eq.eqv(left, right))
      case _ => throw new RuntimeException("This is unexpected, really")
    }
  }

  test("commutativity") {
    forAll(Gen.listOfN(2, counterGen)) {
      case x :: y :: Nil =>
        val left = x |+| y
        val right = y |+| x
        assert(eq.eqv(left, right))
      case _ => throw new RuntimeException("This is unexpected, really")
    }
  }

  test("idempotency") {
    forAll(Gen.listOf(counterGen)) { list =>
      whenever(list.nonEmpty) {
        val counter = list.reduce(_ |+| _)
        assert(eq.eqv(counter, counter |+| counter))
      }
    }
  }
}
