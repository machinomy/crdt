package com.machinomy.crdt.op

import scala.language.implicitConversions
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._

object GraphTypes {
  implicit def intDiGraphToGraphBound(graph: Graph[Int, DiEdge]): GraphProxy = graphToScalaDiGraphProxy(graph)

  def graphToScalaDiGraphProxy[A](_graph: Graph[A, DiEdge]) = new ScalaDiGraphProxy[A] { val graph = _graph }

  trait GraphProxy {
    type V
    type E
    type G
    type P

    def graph: G
  }

  trait ScalaDiGraphProxy[A] extends GraphProxy {
    override type V = A
    override type E = DiEdge[A]
    override type G = Graph[A, DiEdge]
    override type P = G#Path
  }

  trait GraphLike[G <: GraphProxy] {
    def contains(graph: G#G, vertex: G#V): Boolean
    def addEdge(graph: G#G, edge: G#E): G#G
    def addVertex(graph: G#G, vertex: G#V): G#G
    def fromVertex(graph: G#G, edge: G#E): G#V
    def toVertex(graph: G#G, edge: G#E): G#V
    def path(graph: G#G, from: G#V, to: G#V): Option[G#P]
    def existsPath(graph: G#G, from: G#V, to: G#V): Boolean
    def toProxy(graph: G#G): G
  }

  class ScalaDiGraphLike[A] extends GraphLike[ScalaDiGraphProxy[A]] {
    override def contains(graph: Graph[A, DiEdge], vertex: A): Boolean =
      graph.contains(vertex)
    override def addEdge(graph: Graph[A, DiEdge], edge: DiEdge[A]): Graph[A, DiEdge] =
      graph + edge
    override def addVertex(graph: Graph[A, DiEdge], vertex: A): Graph[A, DiEdge] =
      graph + vertex
    override def fromVertex(graph: Graph[A, DiEdge], edge: DiEdge[A]): A =
      edge.from
    override def toVertex(graph: Graph[A, DiEdge], edge: DiEdge[A]): A =
      edge.to
    override def existsPath(graph: Graph[A, DiEdge], from: A, to: A): Boolean =
      path(graph, from, to).isDefined
    override def path(graph: Graph[A, DiEdge], from: A, to: A): Option[graph.Path] =
      for {
        fromVertex <- graph.find(from)
        toVertex <- graph.find(to)
        path <- fromVertex.pathTo(toVertex)
      } yield path

    override def toProxy(graph: Graph[A, DiEdge]): ScalaDiGraphProxy[A] = graphToScalaDiGraphProxy(graph)
  }

  implicit object IntScalaDiGraphLike extends ScalaDiGraphLike[Int]
}
