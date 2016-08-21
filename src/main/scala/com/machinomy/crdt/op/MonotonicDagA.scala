package com.machinomy.crdt.op

import com.machinomy.crdt.op.GraphLike._
import com.sun.tools.corba.se.idl.InvalidArgument

import scala.annotation.tailrec

/**
  * Add-only Monotonic DAG
  */
case class MonotonicDagA[A, V <: VertexLike[A], E <: EdgeLike[A, V]](state: GraphLike[A, V, E]) {
  def contains(v: V) = state.vertices.contains(v)
  def contains(e: E) = state.edges.contains(e)

  def value = state

  def add(e: E): MonotonicDagA.UpdateResult[A, V, E] =
    if(contains(e.u) && contains(e.v) && state.existsPath(e.u, e.v)) {
      val next = MonotonicDagA(state.add(e))
      (next, Some(MonotonicDagA.AddEdge[A, V, E](e)))
    } else {
      (this, None)
    }

  def add(v: V, left: V, right: V)(implicit eb: CanBuildEdge[A, V, E]): MonotonicDagA.UpdateResult[A, V, E] =
    if (!contains(v) && contains(left) && contains(right) && state.existsPath(left, right)) {
      val nextState = state.add(v).add(eb.buildEdge(left, v)).add(eb.buildEdge(v, right))
      val next = MonotonicDagA(state)
      (next, Some(MonotonicDagA.AddVertex(v, left, right)))
    } else {
      (this, None)
    }

  def run(operation: MonotonicDagA.AddEdge[A, V, E]): MonotonicDagA.UpdateResult[A, V, E] = add(operation.e)

  def run(operation: MonotonicDagA.AddVertex[A, V])(implicit eb: CanBuildEdge[A, V, E]): MonotonicDagA.UpdateResult[A, V, E] = add(operation.v, operation.left, operation.right)
}

object MonotonicDagA {
  type UpdateResult[A, V <: VertexLike[A], E <: EdgeLike[A, V]] = (MonotonicDagA[A, V, E], Option[Update[A]])

  sealed trait Update[A]
  case class AddEdge[A, V <: VertexLike[A], E <: EdgeLike[A, V]](e: E) extends Update[A]
  case class AddVertex[A, V <: VertexLike[A]](v: V, left: V, right: V) extends Update[A]
}