package com.machinomy.crdt.op

import com.machinomy.crdt.op.GraphLike._
import com.machinomy.crdt.state.TPSet

/**
  * Add-Remove Partial Order
  */
case class PartialOrderDag[A, V <: VertexLike[A], E <: EdgeLike[A, V]](vertices: TPSet[V], edges: Set[E]) {
  def contains(v: V): Boolean = vertices.value.contains(v)
  def value(implicit gb: CanBuildGraph[A, V, E]): GraphLike[A, V, E] = gb.buildGraph(vertices.value, edges)

  def add(v: V, left: V, right: V)(implicit eb: CanBuildEdge[A, V, E], gb: CanBuildGraph[A, V, E]): PartialOrderDag.UpdateResult[A, V, E] =
    if (!contains(v) && value.existsPath(left, right)) {
      val nextVertices = vertices + v
      val leftEdge = eb.buildEdge(left, v)
      val rightEdge = eb.buildEdge(v, right)
      val nextEdges = edges + leftEdge + rightEdge
      val next = new PartialOrderDag[A, V, E](nextVertices, nextEdges)
      (next, Some(PartialOrderDag.AddVertex[A, V](v, left, right)))
    } else {
      (this, None)
    }

  def remove(v: V): PartialOrderDag.UpdateResult[A, V, E] =
    if (contains(v) && !v.isInstanceOf[SentinelLike[A]]) {
      val nextVertices = vertices - v
      val next = new PartialOrderDag[A, V, E](nextVertices, edges)
      (next, Some(PartialOrderDag.RemoveVertex[A, V](v)))
    } else {
      (this, None)
    }

  def run(operation: PartialOrderDag.AddVertex[A, V])(implicit eb: CanBuildEdge[A, V, E], gb: CanBuildGraph[A, V, E]) =
    add(operation.v, operation.left, operation.right)

  def run(operation: PartialOrderDag.RemoveVertex[A, V]) =
    remove(operation.v)
}

object PartialOrderDag {
  type UpdateResult[A, V <: VertexLike[A], E <: EdgeLike[A, V]] = (PartialOrderDag[A, V, E], Option[Update[A]])

  sealed trait Update[A]
  case class AddVertex[A, V <: VertexLike[A]](v: V, left: V, right: V) extends Update[A]
  case class RemoveVertex[A, V <: VertexLike[A]](v: V) extends Update[A]
}
