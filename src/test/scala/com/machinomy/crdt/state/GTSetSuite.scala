package com.machinomy.crdt.state

import cats._
import cats.syntax.all._
import com.github.nscala_time.time.Imports._
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
    val a = Monoid[GTSet[Int, DateTime]].empty + 1 + 2 + 3
    val b = Monoid[GTSet[Int, DateTime]].empty + 2 + 3 + 4
    val result = a |+| b
    assert(result.value === Set(1, 2, 3, 4))
  }
}
