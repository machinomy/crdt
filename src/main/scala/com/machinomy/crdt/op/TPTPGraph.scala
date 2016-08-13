package com.machinomy.crdt.op

import com.machinomy.crdt.op.Graph._

/**
  * 2P2P-Graph
  */
case class TPTPGraph[A, V <: VertexLike[A], E <: EdgeLike[A, V]](va: Set[V], vr: Set[V], ea: Set[E], er: Set[E]) {
  type Self = TPTPGraph[A, V, E]

  def contains(v: V): Boolean = vertices.contains(v)

  def contains(e: E): Boolean = contains(e.u) && contains(e.v) && (ea -- er).contains(e)

  def add(v: V): TPTPGraph.UpdateResult[A, V, E] = {
    val nextVa = va + v
    val next: Self = copy(va = nextVa)
    (next, Some(TPTPGraph.AddVertex(v)))
  }

  def add(e: E): TPTPGraph.UpdateResult[A, V, E] = {
    if (contains(e.u) && contains(e.v)) {
      val nextEa = ea + e
      val next: Self = copy(ea = nextEa)
      (next, Some(TPTPGraph.AddEdge[A, V, E](e)))
    } else {
      (this, None)
    }
  }

  def isSingle(v: V): Boolean = (ea -- er).forall(e => e.u != v && e.v != v)

  def remove(v: V): TPTPGraph.UpdateResult[A, V, E] = {
    if (contains(v) && isSingle(v)) {
      val nextVr = vr + v
      val next: Self = copy(vr = nextVr)
      (next, Some(TPTPGraph.RemoveVertex[A, V](v)))
    } else {
      (this, None)
    }
  }

  def remove(e: E): TPTPGraph.UpdateResult[A, V, E] = {
    if (contains(e)) {
      val nextEr = er + e
      val next: Self = copy(er = nextEr)
      (next, Some(TPTPGraph.RemoveEdge[A, V, E](e)))
    } else {
      (this, None)
    }
  }

  def vertices: Set[V] = va -- vr

  def edges: Set[E] = ea -- er
}

object TPTPGraph {
  type UpdateResult[A, V <: VertexLike[A], E <: EdgeLike[A, V]] = (TPTPGraph[A, V, E], Option[TPTPGraph.Update[A]])

  def apply[A, V <: VertexLike[A], E <: EdgeLike[A, V]](): TPTPGraph[A, V, E] = {
    val va = Set.empty[V]
    val vr = Set.empty[V]
    val ea = Set.empty[E]
    val er = Set.empty[E]
    new TPTPGraph(va, vr, ea, er)
  }

  sealed trait Update[A]
  case class AddVertex[A, V <: VertexLike[A]](v: V) extends Update[A]
  case class AddEdge[A, V <: VertexLike[A], E <: EdgeLike[A, V]](e: E) extends Update[A]
  case class RemoveVertex[A, V <: VertexLike[A]](v: V) extends Update[A]
  case class RemoveEdge[A, V <: VertexLike[A], E <: EdgeLike[A, V]](e: E) extends Update[A]
}
