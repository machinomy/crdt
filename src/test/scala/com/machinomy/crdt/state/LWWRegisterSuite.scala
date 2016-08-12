package com.machinomy.crdt.state

import org.joda.time.DateTime
import org.scalatest.FunSuite
import com.github.nscala_time.time.Imports._
import cats.syntax.all._

class LWWRegisterSuite extends FunSuite {
  test("value") {
    val register = LWWRegister[Int, DateTime](0)
    assert(register.value == 0)
  }

  test("merge") {
    val a = LWWRegister[Int, DateTime](0)
    val b = LWWRegister[Int, DateTime](1, DateTime.now + 10.seconds)
    val c = a |+| b
    assert(c.value == 1)
  }
}
