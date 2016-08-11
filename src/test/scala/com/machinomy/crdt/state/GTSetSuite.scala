package com.machinomy.crdt.state

import org.joda.time.DateTime
import org.scalatest.FunSuite

class GTSetSuite extends FunSuite {
  test("Just created GTSet is empty") {
    val set = new GTSet[Int, DateTime]()
    assert(set.value.isEmpty)
  }

  test("GTSet calculates value") {
    val a = new GTSet[Int, DateTime]()
    val b = a + 3
    val c = b + 1
    val d = c + 3
    assert(d.value === Set(1, 3))
  }

  test("GTSets can be merged") {
    val a = new GTSet[Int, DateTime] + 1 + 2 + 3
    val b = new GTSet[Int, DateTime] + 2 + 3 + 4
    val result = a.merge(b)
    assert(result.value === Set(1, 2, 3, 4))
  }
}
