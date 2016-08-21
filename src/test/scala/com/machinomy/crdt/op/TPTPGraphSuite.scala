package com.machinomy.crdt.op

import com.machinomy.crdt.op.GraphLike._
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
    op.get.isInstanceOf[TPTPGraph.AddVertex[_, _]] should be(true)
  }

  test("add vertex operation") {
    val g = TPTPGraph[Int, Vertex, Edge]()
    val vertex = Vertex(3)
    val addOp = TPTPGraph.AddVertex[Int, Vertex](vertex)
    val (g2, op) = g.run(addOp)
    g2.edges should be(empty)
    g2.vertices should contain(vertex)
    op shouldNot be(empty)
    op.get.isInstanceOf[TPTPGraph.AddVertex[_, _]] should be(true)
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
    op4.get.isInstanceOf[TPTPGraph.AddEdge[_, _, _]] should be(true)
  }

  test("add edge operation") {
    val g = TPTPGraph[Int, Vertex, Edge]()
    val u = Vertex(3)
    val v = Vertex(4)
    val addOp1 = TPTPGraph.AddVertex[Int, Vertex](u)
    val (g2, op2) = g.add(u)
    val (g3, op3) = g2.add(v)
    val e = Edge(u, v)
    val (g4, op4) = g3.add(e)

    g4.edges should contain(e)
    g4.vertices should contain(u)
    g4.vertices should contain(v)
    op4 shouldNot be(empty)
    op4.get.isInstanceOf[TPTPGraph.AddEdge[_, _, _]] should be(true)
  }

  test("remove vertex") {
    val g = TPTPGraph[Int, Vertex, Edge]()
    val v = Vertex(3)
    val (g2, op2) = g.add(v)
    val (g3, op3) = g2.remove(v)

    g3.edges should be(empty)
    g3.vertices should be(empty)
  }

  test("remove vertex operation") {
    val g = TPTPGraph[Int, Vertex, Edge]()
    val v = Vertex(3)
    val (g2, op2) = g.add(v)
    val removeOp = TPTPGraph.RemoveVertex[Int, Vertex](v)
    val (g3, op3) = g2.remove(v)

    g3.edges should be(empty)
    g3.vertices should be(empty)
  }

  test("remove edge") {
    val g = TPTPGraph[Int, Vertex, Edge]()
    val u = Vertex(3)
    val v = Vertex(4)
    val (g2, op2) = g.add(u)
    val (g3, op3) = g2.add(v)
    val e = Edge(u, v)
    val (g4, op4) = g3.add(e)
    val (g5, op5) = g4.remove(e)

    g5.edges shouldNot contain(e)
    g4.vertices should contain(u)
    g4.vertices should contain(v)
    op5 shouldNot be(empty)
    op5.get.isInstanceOf[TPTPGraph.RemoveEdge[_, _, _]] should be(true)
  }

  test("remove edge operation") {
    val g = TPTPGraph[Int, Vertex, Edge]()
    val u = Vertex(3)
    val v = Vertex(4)
    val (g2, op2) = g.add(u)
    val (g3, op3) = g2.add(v)
    val e = Edge(u, v)
    val (g4, op4) = g3.add(e)
    val removeOp = TPTPGraph.RemoveEdge[Int, Vertex, Edge](e)
    val (g5, op5) = g4.run(removeOp)

    g5.edges shouldNot contain(e)
    g4.vertices should contain(u)
    g4.vertices should contain(v)
    op5 shouldNot be(empty)
    op5.get.isInstanceOf[TPTPGraph.RemoveEdge[_, _, _]] should be(true)
  }
}
