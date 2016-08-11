package com.machinomy.crdt.state

import com.github.nscala_time.time.Imports._
import org.scalatest.FunSuite

class ORSetSuite extends FunSuite {
  test("Fresh ORSet is empty") {
    val fresh = ORSet[Int, DateTime]()
    assert(fresh.value.isEmpty)
  }

  test("ORSet could be updated") {
    val a = ORSet[Int, DateTime]() + 3
    assert(a.value === Set(3))

    val b = ORSet[Int, DateTime]() + 3 - 3
    assert(b.value === Set.empty)

    val now = DateTime.now()
    val c = ORSet[Int, DateTime]() + 3 - 3
    assert(c.value === Set.empty)

    val d = c + (3, now + 10.minutes)
    assert(d.value === Set(3))
  }

}
