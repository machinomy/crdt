package com.machinomy.crdt.op

import com.machinomy.crdt.op.Graph._
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks

class TPTPGraphSuite extends FunSuite with PropertyChecks with Matchers {
  case class Vertex(id: Int) extends VertexLike[Int]
  case class Edge(u: Vertex, v: Vertex) extends EdgeLike[Int, Vertex]

  test("initial") {
    val g = TPTPGraph[Int, Vertex, Edge]()
    g.edges should be(empty)
    g.vertices should be(empty)
  }

  test("add vertex") {
    val g = TPTPGraph[Int, Vertex, Edge]()
    val vertex = Vertex(3)
    val (g2, op) = g.add(vertex)
    g2.edges should be(empty)
    g2.vertices should contain(vertex)
    op shouldNot be(empty)
    op.get.isInstanceOf[TPTPGraph.AddVertex[Int, Vertex]] should be(true)
  }

  test("add edge") {
    val g = TPTPGraph[Int, Vertex, Edge]()
    val u = Vertex(3)
    val v = Vertex(4)
    val (g2, op2) = g.add(u)
    val (g3, op3) = g2.add(v)
    val e = Edge(u, v)
    val (g4, op4) = g3.add(e)

    g4.edges should contain(e)
    g4.vertices should contain(u)
    g4.vertices should contain(v)
    op4 shouldNot be(empty)
    op4.get.isInstanceOf[TPTPGraph.AddEdge[Int, Vertex, Edge]] should be(true)
  }
}
