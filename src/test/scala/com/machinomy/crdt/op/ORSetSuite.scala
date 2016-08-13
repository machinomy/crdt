package com.machinomy.crdt.op

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import com.github.nscala_time.time.Imports._
import com.machinomy.crdt.state.TombStone

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
      operation.get.isInstanceOf[ORSet.Add[_, _]] should be(true)
    }
  }

  test("remove if present") {
    forAll(Gen.posNum[Int]) { (i: Int) =>
      val initial = ORSet[Int, DateTime]()
      val (set, _) = initial.add(i)
      val (finalSet, operation) = set.remove(i)
      finalSet.value should be(empty)
      operation.isDefined should be(true)
      operation.get.isInstanceOf[ORSet.Remove[_, _]] should be(true)
    }
  }

  test("remove if absent") {
    forAll(Gen.posNum[Int]) { (i: Int) =>
      val initial = ORSet[Int, DateTime]()
      val (set, operation) = initial.remove(i)
      set.value should be(empty)
      operation shouldNot be(empty)
      operation.get.isInstanceOf[ORSet.Remove[_, _]] should be(true)
    }
  }

  test("add operation") {
    forAll(Gen.posNum[Int]) { (i: Int) =>
      val set = ORSet[Int, DateTime]()
      val addOp = ORSet.Add(i, implicitly[TombStone[DateTime]].next)
      val (nextSet, operation) = set.run(addOp)
      nextSet.value should be(Set(i))
      operation shouldNot be(empty)
      operation.get.isInstanceOf[ORSet.Add[_, _]] should be(true)
    }
  }

  test("remove operation if present") {
    forAll(Gen.posNum[Int]) { (i: Int) =>
      val initial = ORSet[Int, DateTime]()
      val (set, _) = initial.add(i)
      val removeOp = ORSet.Remove(i, set.state(i))
      val (finalSet, operation) = set.run(removeOp)
      finalSet.value should be(empty)
      operation.isDefined should be(true)
      operation.get.isInstanceOf[ORSet.Remove[_, _]] should be(true)
    }
  }

  test("remove operation if absent") {
    forAll(Gen.posNum[Int]) { (i: Int) =>
      val initial = ORSet[Int, DateTime]()
      val removeOp = ORSet.Remove(i, Set(implicitly[TombStone[DateTime]].next))
      val (set, operation) = initial.run(removeOp)
      set.value should be(empty)
      operation should be(empty)
    }
  }
}
