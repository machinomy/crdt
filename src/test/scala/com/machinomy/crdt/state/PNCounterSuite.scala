package com.machinomy.crdt.state

import org.scalatest.FunSuite

class PNCounterSuite extends FunSuite {
  test("Empty value is zero") {
    val fresh = PNCounter[Int, Int]()
    assert(fresh.value === 0)
  }

  test("Could be calculated") {
    val counter: PNCounter[Int, Int] = PNCounter[Int, Int]() + (1, 1) + (2, 3)
    assert(counter.value === 1 + 3)
  }

  test("Could be merged") {
    val a = PNCounter[Int, Int]() + (1, 1) + (2, 3)
    val b = PNCounter[Int, Int]() + (1, 2) + (2, -3)
    val c = a.merge(b)
    assert(c.value === 2)
  }

  test("Could get replica value") {
    val a = PNCounter[Int, Int]() + (1, 2) + (2, 3) + (1, -1)
    assert(a.get(1) === 1)
  }

  test("Could get table") {
    val a = PNCounter[Int, Int]() + (1, 2) + (2, 3) + (1, -1)
    assert(a.table === Map(1 -> 1, 2 -> 3))
  }

  test("Can update table") {
    val a = PNCounter[Int, Int]() + (1, 2) + (2, 3) + (1, -1)
    val b = a + (2, 5)
    assert(b.table === Map(1 -> 1, 2 -> 8))
  }
}
