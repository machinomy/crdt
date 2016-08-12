package com.machinomy.crdt.state

import com.github.nscala_time.time.Imports._
import org.scalatest.FunSuite
import cats.syntax.all._

class LWWElementSetSuite extends FunSuite {
  test("Fresh LWWElementSet is empty") {
    val fresh = LWWElementSet[Int, DateTime, Bias.AdditionWins]()
    assert(fresh.value.isEmpty)
  }

  test("LWWElementSet could be updated") {
    val a = LWWElementSet[Int, DateTime, Bias.AdditionWins]() + 3 - (3, DateTime.now + 10.minutes)
    assert(a.value === Set.empty)

    val now = DateTime.now()
    val b = LWWElementSet[Int, DateTime, Bias.AdditionWins]() + (3, now) - (3, now - 10.minutes)
    assert(b.value === Set(3))
  }

  test("LWWElementSet could be biased to additions") {
    val now = DateTime.now()
    val b = LWWElementSet[Int, DateTime, Bias.AdditionWins]() + (3, now) - (3, now)
    assert(b.value === Set(3))
  }

  test("LWWElementSet could be biased to removals") {
    val now = DateTime.now()
    val b = LWWElementSet[Int, DateTime, Bias.RemovalWins]() + (3, now) - (3, now)
    assert(b.value === Set.empty)
  }

  test("LWWElementSet could be merged") {
    val now = DateTime.now()
    val a = LWWElementSet[Int, DateTime, Bias.AdditionWins]() + (3, now) - (3, now - 10.minutes)
    val b = LWWElementSet[Int, DateTime, Bias.AdditionWins]() + (1, now) - (1, now + 2.minutes) + (2, now)
    val c = a |+| b
    assert(c.value === Set(3, 2))
  }
}
