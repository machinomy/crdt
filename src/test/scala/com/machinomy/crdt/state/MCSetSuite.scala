package com.machinomy.crdt.state

import org.scalatest.FunSuite
import cats.syntax.all._

class MCSetSuite extends FunSuite {
  test("Just created MCSet is empty") {
    val gSet = MCSet[Int, Int]()
    assert(gSet.value.isEmpty)
  }

  test("MCSet calculates value") {
    val a = MCSet[Int, Int]() + 3 + 1 + 3
    assert(a.value === Set(1, 3))
  }

  test("MCSet additions can be merged") {
    val a = new MCSet[Int, Int] + 1 + 2 + 3
    val b = new MCSet[Int, Int] + 2 + 3 + 4
    val result = a |+| b
    assert(result.value === Set(1, 2, 3, 4))
  }

  test("MCSet additions and removals can be merged") {
    val a = new MCSet[Int, Int] + 1 + 2 + 3
    val b = new MCSet[Int, Int] - 2 - 3 - 4
    val c = a |+| b
    assert(c.value === Set(1, 2, 3))

    val d = a |+| (a - 2 - 3)
    assert(d.value === Set(1))
  }
}
