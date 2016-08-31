package com.machinomy.crdt.state

import cats._
import cats.syntax.all._
import org.scalatest.FunSuite

class MCSetSuite extends FunSuite {
  test("Just created MCSet is empty") {
    val gSet = Monoid[MCSet[Int, Int]].empty
    assert(gSet.value.isEmpty)
  }

  test("MCSet calculates value") {
    val a = Monoid[MCSet[Int, Int]].empty + 3 + 1 + 3
    assert(a.value === Set(1, 3))
  }

  test("MCSet additions can be merged") {
    val a = Monoid[MCSet[Int, Int]].empty + 1 + 2 + 3
    val b = Monoid[MCSet[Int, Int]].empty + 2 + 3 + 4
    val result = a |+| b
    assert(result.value === Set(1, 2, 3, 4))
  }

  test("MCSet additions and removals can be merged") {
    val a = Monoid[MCSet[Int, Int]].empty + 1 + 2 + 3
    val b = Monoid[MCSet[Int, Int]].empty - 2 - 3 - 4
    val c = a |+| b
    assert(c.value === Set(1, 2, 3))

    val d = a |+| (a - 2 - 3)
    assert(d.value === Set(1))
  }
}
