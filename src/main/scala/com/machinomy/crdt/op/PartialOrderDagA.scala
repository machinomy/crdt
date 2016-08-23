package com.machinomy.crdt.op

import com.machinomy.crdt.op.GraphLikeA._
import com.machinomy.crdt.state.TPSet

/**
  * Add-Remove Partial Order
  */
case class PartialOrderDagA[A, V <: VertexLike[A], E <: EdgeLike[A, V]](vertices: TPSet[V], edges: Set[E]) {
  def contains(v: V): Boolean = vertices.value.contains(v)
  def value(implicit gb: CanBuildGraph[A, V, E]): GraphLike[A, V, E] = gb.buildGraph(vertices.value, edges)

  def add(v: V, left: V, right: V)(implicit eb: CanBuildEdge[A, V, E], gb: CanBuildGraph[A, V, E]): PartialOrderDagA.UpdateResult[A, V, E] =
    if (!contains(v) && value.existsPath(left, right)) {
      val nextVertices = vertices + v
      val leftEdge = eb.buildEdge(left, v)
      val rightEdge = eb.buildEdge(v, right)
      val nextEdges = edges + leftEdge + rightEdge
      val next = new PartialOrderDagA[A, V, E](nextVertices, nextEdges)
      (next, Some(PartialOrderDagA.AddVertex[A, V](v, left, right)))
    } else {
      (this, None)
    }

  def remove(v: V): PartialOrderDagA.UpdateResult[A, V, E] =
    if (contains(v) && !v.isInstanceOf[SentinelLike[A]]) {
      val nextVertices = vertices - v
      val next = new PartialOrderDagA[A, V, E](nextVertices, edges)
      (next, Some(PartialOrderDagA.RemoveVertex[A, V](v)))
    } else {
      (this, None)
    }

  def run(operation: PartialOrderDagA.AddVertex[A, V])(implicit eb: CanBuildEdge[A, V, E], gb: CanBuildGraph[A, V, E]) =
    add(operation.v, operation.left, operation.right)

  def run(operation: PartialOrderDagA.RemoveVertex[A, V]) =
    remove(operation.v)
}

object PartialOrderDagA {
  type UpdateResult[A, V <: VertexLike[A], E <: EdgeLike[A, V]] = (PartialOrderDagA[A, V, E], Option[Update[A]])

  sealed trait Update[A]
  case class AddVertex[A, V <: VertexLike[A]](v: V, left: V, right: V) extends Update[A]
  case class RemoveVertex[A, V <: VertexLike[A]](v: V) extends Update[A]
}
