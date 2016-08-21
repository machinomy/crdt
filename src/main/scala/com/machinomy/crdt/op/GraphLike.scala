package com.machinomy.crdt.op

import scala.language.higherKinds

object GraphLike {
  trait VertexLike[A]
  trait SentinelLike[A] extends VertexLike[A]
  trait EdgeLike[A, V <: VertexLike[A]] {
    def u: V
    def v: V
  }
  trait GraphLike[A, V <: VertexLike[A], E <: EdgeLike[A, V]] {
    def vertices: Set[V]
    def edges: Set[E]
    def existsPath(u: V, v: V): Boolean
    def add(e: E): GraphLike[A, V, E]
    def add(v: V): GraphLike[A, V, E]
  }
  trait CanBuildEdge[A, V <: VertexLike[A], E <: EdgeLike[A, V]] {
    def buildEdge(u: V, v: V): E
  }
  trait CanBuildGraph[A, V <: VertexLike[A], E <: EdgeLike[A, V]] {
    def buildGraph(vertices: Set[V], edges: Set[E]): GraphLike[A, V, E]
  }
}
