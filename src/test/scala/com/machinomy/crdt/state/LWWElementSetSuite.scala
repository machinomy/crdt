package com.machinomy.crdt.state

import cats.syntax.all._
import com.github.nscala_time.time.Imports._
import org.scalatest.FunSuite

class LWWElementSetSuite extends FunSuite {
  test("Fresh LWWElementSetA is empty") {
    val fresh = LWWElementSetA[Int, DateTime, Bias.AdditionWins]()
    assert(fresh.value.isEmpty)
  }

  test("LWWElementSetA could be updated") {
    val a = LWWElementSetA[Int, DateTime, Bias.AdditionWins]() + 3 - (3, DateTime.now + 10.minutes)
    assert(a.value === Set.empty)

    val now = DateTime.now()
    val b = LWWElementSetA[Int, DateTime, Bias.AdditionWins]() + (3, now) - (3, now - 10.minutes)
    assert(b.value === Set(3))
  }

  test("LWWElementSetA could be biased to additions") {
    val now = DateTime.now()
    val b = LWWElementSetA[Int, DateTime, Bias.AdditionWins]() + (3, now) - (3, now)
    assert(b.value === Set(3))
  }

  test("LWWElementSetA could be biased to removals") {
    val now = DateTime.now()
    val b = LWWElementSetA[Int, DateTime, Bias.RemovalWins]() + (3, now) - (3, now)
    assert(b.value === Set.empty)
  }

  test("LWWElementSetA could be merged") {
    val now = DateTime.now()
    val a = LWWElementSetA[Int, DateTime, Bias.AdditionWins]() + (3, now) - (3, now - 10.minutes)
    val b = LWWElementSetA[Int, DateTime, Bias.AdditionWins]() + (1, now) - (1, now + 2.minutes) + (2, now)
    val c = a |+| b
    assert(c.value === Set(3, 2))
  }
}
