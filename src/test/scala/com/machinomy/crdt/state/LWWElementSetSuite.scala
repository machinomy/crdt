package com.machinomy.crdt.state

import cats._
import cats.syntax.all._
import com.github.nscala_time.time.Imports._
import org.scalatest.FunSuite

class LWWElementSetSuite extends FunSuite {
  test("fresh is empty") {
    val set = Monoid[LWWElementSet[Int, DateTime, Bias.AdditionWins]].empty
    assert(set.value.isEmpty)
  }

  test("calculates value") {
    val a = Monoid[LWWElementSet[Int, DateTime, Bias.AdditionWins]].empty + 3 + 1 + 3
    assert(a.value === Set(1, 3))
  }

  test("can be combined, addition bias") {
    val now = DateTime.now
    val a = Monoid[LWWElementSet[Int, DateTime, Bias.AdditionWins]].empty + (1, now) + (2, now) + (3, now)
    val b = Monoid[LWWElementSet[Int, DateTime, Bias.AdditionWins]].empty - (2, now) - (3, now) - (4, now)
    val result = a |+| b
    assert(result.value === Set(1, 2, 3))
  }

  test("can be combined, removal bias") {
    val now = DateTime.now
    val a = Monoid[LWWElementSet[Int, DateTime, Bias.RemovalWins]].empty + (1, now) + (2, now) + (3, now)
    val b = Monoid[LWWElementSet[Int, DateTime, Bias.RemovalWins]].empty - (2, now) - (3, now) - (4, now)
    val result = a |+| b
    assert(result.value === Set(1))
  }
}
