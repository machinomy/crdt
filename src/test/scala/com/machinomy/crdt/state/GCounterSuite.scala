package com.machinomy.crdt.state

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import cats.kernel.{Eq, Monoid}
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import cats.syntax.all._
import com.machinomy.crdt.state.GCounterSuite.Forward

import scala.collection.immutable.IndexedSeq
import scala.concurrent.Await
import scala.concurrent.duration._

class GCounterSuite extends FunSuite with PropertyChecks {
  val replicaGen = Gen.posNum[Int]
  val valueGen = Gen.posNum[Int]
  val counterGen = for {
    id <- replicaGen
    value <- valueGen
  } yield GCounter[Int, Int]() + (id -> value)
  val eq = implicitly[Eq[GCounter[Int, Int]]]

  test("Fresh GCounter is empty") {
    val fresh = GCounter[Int, Int]()
    assert(Monoid[GCounter[Int, Int]].isEmpty(fresh))
    assert(fresh.value === 0)
  }

  test("could be calculated") {
    val counter = GCounter[Int, Int]().increment(1, 2).increment(2, 3)
    assert(counter.value === 5)
  }

  test("could be combined") {
    val a = GCounter[Int, Int]() + (1, 2) + (2, 3)
    val b = GCounter[Int, Int]() + (1, 2) + (3, 4)
    val c = a |+| b
    assert(c.value === 2 + 3 + 4)
  }

  test("could present replica value") {
    val a = GCounter[Int, Int]()
    assert(a.get(0) === 0)
    val b = a.increment(1, 2).increment(2, 3)
    assert(b.get(1) === 2)
    assert(b.get(2) === 3)
  }

  test("equality") {
    val a = GCounter[Int, Int]()
    val b = GCounter[Int, Int]()
    assert(eq.eqv(a, b))

    val a1 = a + (1 -> 1)
    assert(eq.neqv(a1, b))
    val b1 = b + (1 -> 2)
    assert(a1 !== b1)
    assert(eq.neqv(a1, b1))
    val a2 = a1 + (1 -> 1)
    assert(eq.eqv(a2, b1))
  }

  test("associativity") {
    forAll(Gen.listOfN(3, counterGen)) {
      case x :: y :: z :: Nil =>
        val left = x |+| (y |+| z)
        val right = (x |+| y) |+| z
        assert(eq.eqv(left, right))
      case _ => throw new RuntimeException("This is unexpected, really")
    }
  }

  test("commutativity") {
    forAll(Gen.listOfN(2, counterGen)) {
      case x :: y :: Nil =>
        val left = x |+| y
        val right = y |+| x
        assert(eq.eqv(left, right))
      case _ => throw new RuntimeException("This is unexpected, really")
    }
  }

  test("idempotency") {
    forAll(Gen.listOf(counterGen)) { list =>
      whenever(list.nonEmpty) {
        val counter = list.reduce(_ |+| _)
        val sum = counter |+| counter
        assert(eq.eqv(counter, sum))
      }
    }
  }

  test("bounce") {
    val system = ActorSystem("gcounter")
    val actors: IndexedSeq[(ActorRef, Int)] = for (i <- 1 to 10) yield (system.actorOf(GCounterSuite.Pass.props(i)), valueGen.sample.getOrElse(1))
    var counter = GCounter[Int, Int]()
    var sum: Int= 0
    implicit val timeout = Timeout(3.seconds)
    for ((actor, rand) <- actors) {
      val response = actor.ask(Forward(counter, rand)).mapTo[GCounter[Int, Int]]
      counter = Await.result[GCounter[Int, Int]](response, timeout.duration)
      sum += rand
    }
    assert(counter.value === sum)
  }
}

object GCounterSuite {
  class Pass(id: Int) extends Actor {
    override def receive: Receive = {
      case Forward(counter, number) =>
        val nextCounter = counter + (id -> number)
        sender ! nextCounter
    }
  }

  object Pass {
    def props(id: Int) = Props(classOf[Pass], id)
  }

  case class Forward(counter: GCounter[Int, Int], number: Int)
}
