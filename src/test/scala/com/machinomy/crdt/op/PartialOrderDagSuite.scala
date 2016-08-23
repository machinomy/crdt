package com.machinomy.crdt.op

import com.machinomy.crdt.state.TPSet
import org.scalatest.{FunSuite, Matchers}

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

class PartialOrderDagSuite extends FunSuite with Matchers {
  test("add, edge") {
    val g: Graph[Int, DiEdge] = Graph[Int, DiEdge]() + 1 ~> 100
    val edges: Set[DiEdge[Int]] = g.edges.toOuter
    val vertices = TPSet(g.nodes.toOuter)
    val dag = PartialOrderDag[Int, DiEdge](vertices, edges)
    val (dag2, op) = dag.add(2, 1, 100)
    dag2.value.edges shouldNot be(empty)
  }

  test("add, vertex") {
    /*val g: Graph[Int, DiEdge] = Graph[Int, DiEdge]() + 1 + 100
    val (dag, _)= MonotonicDag[Int, DiEdge, Graph[Int, DiEdge]](g).add(1 ~> 100)
    val (dag2, op) = dag.add(2, 1, 100)
    dag2.value.edges shouldNot be(empty)*/
  }
}
