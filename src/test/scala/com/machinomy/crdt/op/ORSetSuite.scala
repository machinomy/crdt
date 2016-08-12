package com.machinomy.crdt.op

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import com.github.nscala_time.time.Imports._

class ORSetSuite extends FunSuite with PropertyChecks with Matchers {
  test("fresh is empty") {
    val set = ORSet[Int, DateTime]()
    assert(set.value.isEmpty)
  }

  test("add") {
    forAll(Gen.posNum[Int]) { (i: Int) =>
      val set = ORSet[Int, DateTime]()
      val (nextSet, operation) = set.add(i)
      nextSet.value should be(Set(i))
      operation.isDefined should be(true)
      operation.get.isInstanceOf[ORSet.Add[Int, DateTime]] should be(true)
    }
  }

  test("remove if present") {
    forAll(Gen.posNum[Int]) { (i: Int) =>
      val initial = ORSet[Int, DateTime]()
      val (set, _) = initial.add(i)
      val (finalSet, operation) = set.remove(i)
      finalSet.value should be(empty)
      operation.isDefined should be(true)
      operation.get.isInstanceOf[ORSet.Remove[Int, DateTime]] should be(true)
    }
  }

  test("remove if absent") {
    forAll(Gen.posNum[Int]) { (i: Int) =>
      val initial = ORSet[Int, DateTime]()
      val (set, operation) = initial.remove(i)
      set.value should be(empty)
      operation.isInstanceOf[Some[ORSet.Remove[Int, DateTime]]] should be(true)
    }
  }
}
