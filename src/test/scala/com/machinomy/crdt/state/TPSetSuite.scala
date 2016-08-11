package com.machinomy.crdt.state

import org.scalatest.FunSuite

class TPSetSuite extends FunSuite {
  test("Fresh TPSet is empty") {
    val fresh = TPSet[Int]()
    assert(fresh.value.isEmpty)
  }

  test("TPSet could be updated") {
    val a = TPSet[Int]() + 3 - 3
    assert(a.value === Set.empty[Int])
    val b = TPSet[Int]() + 3 - 1
    assert(b.value === Set(3))
  }

  test("TPSet could be merged") {
    val a = TPSet[Int]() + 3 - 3
    val b = TPSet[Int]() + 1 - 1 + 2
    val c = a.merge(b)
    assert(c.value === Set(2))
  }
}
