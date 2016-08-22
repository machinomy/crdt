package com.machinomy.crdt.op

import org.scalatest.{FunSuite, Matchers}

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import GraphProxy._

class MonotonicDagSuite extends FunSuite with Matchers {
  test("add, value") {
    val g: Graph[Int, DiEdge] = Graph[Int, DiEdge]() + 1 + 2
    //val dag = MonotonicDag(g)
    /*val g = Graph[Int, DiEdge]()
    val dag = MonotonicDag(g)
    val edge = 1 ~> 2
    val (dag2, op) = dag.add(edge)
    println(dag2)*/
  }
}
