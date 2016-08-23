package com.machinomy.crdt.op

import org.scalatest.{FunSuite, Matchers}

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

class MonotonicDagSuite extends FunSuite with Matchers {
  test("add, edge") {
    val g: Graph[Int, DiEdge] = Graph[Int, DiEdge]() + 1 + 2
    val dag = MonotonicDag[Int, DiEdge, Graph[Int, DiEdge]](g)
    val edge = 1 ~> 2
    val (dag2, op) = dag.add(edge)
    dag2.value.edges shouldNot be(empty)
  }

  test("add, vertex") {
    val g: Graph[Int, DiEdge] = Graph[Int, DiEdge]() + 1 + 100
    val dag = MonotonicDag[Int, DiEdge, Graph[Int, DiEdge]](g)
    val (dag2, op) = dag.add(2, 1, 100)
    dag2.value.edges shouldNot be(empty)
  }
}
