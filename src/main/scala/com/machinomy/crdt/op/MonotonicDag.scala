package com.machinomy.crdt.op

import com.machinomy.crdt.op.Graph._
import com.sun.tools.corba.se.idl.InvalidArgument

import scala.annotation.tailrec

/**
  * Add-only Monotonic DAG
  */
case class MonotonicDag[A, V <: VertexLike[A], E <: EdgeLike[A, V]](state: GraphLike[A, V, E]) {
  def contains(v: V) = state.vertices.contains(v)
  def contains(e: E) = state.edges.contains(e)

  def value = state

  def add(e: E): MonotonicDag.UpdateResult[A, V, E] =
    if(contains(e.u) && contains(e.v) && state.existsPath(e.u, e.v)) {
      val next = MonotonicDag(state.add(e))
      (next, Some(MonotonicDag.AddEdge[A, V, E](e)))
    } else {
      (this, None)
    }

  def add(v: V, left: V, right: V)(implicit eb: CanBuildEdge[A, V, E]): MonotonicDag.UpdateResult[A, V, E] =
    if (!contains(v) && contains(left) && contains(right) && state.existsPath(left, right)) {
      val nextState = state.add(v).add(eb.buildEdge(left, v)).add(eb.buildEdge(v, right))
      val next = MonotonicDag(state)
      (next, Some(MonotonicDag.AddVertex(v, left, right)))
    } else {
      (this, None)
    }

  def run(operation: MonotonicDag.AddEdge[A, V, E]): MonotonicDag.UpdateResult[A, V, E] = add(operation.e)

  def run(operation: MonotonicDag.AddVertex[A, V]): MonotonicDag.UpdateResult[A, V, E] = add(operation.v, operation.left, operation.right)
}

object MonotonicDag {
  type UpdateResult[A, V <: VertexLike[A], E <: EdgeLike[A, V]] = (MonotonicDag[A, V, E], Option[Update[A]])

  sealed trait Update[A]
  case class AddEdge[A, V <: VertexLike[A], E <: EdgeLike[A, V]](e: E) extends Update[A]
  case class AddVertex[A, V <: VertexLike[A]](v: V, left: V, right: V) extends Update[A]
}
