package com.machinomy.crdt.op

import com.machinomy.crdt.op.PartialOrderDag.Update
import com.machinomy.crdt.state.TPSet

import scalax.collection.Graph
import scalax.collection.GraphPredef._

/**
  * Add-Remove Partial Order
  */
case class PartialOrderDag[V, E[X] <: DiEdgeLikeIn[X], G <: Graph[V, E]](vertices: TPSet[V], edges: Set[E[V] with OuterEdge[V, E]])(implicit graphLike: DiGraphLike[V, E, G]) {
  lazy val value: G = graphLike.buildGraph(vertices.value, edges)

  def add(v: V, left: V, right: V): PartialOrderDag.UpdateResult[V, E, G] =
    if (!value.contains(v) && graphLike.existsPath(value, left, right)) {
      val nextVertices = vertices + v
      val leftEdge = graphLike.buildEdge(left, v)
      val rightEdge = graphLike.buildEdge(v, right)
      val nextEdges = edges + leftEdge + rightEdge
      val next = new PartialOrderDag[V, E, G](nextVertices, nextEdges)
      (next, Some(PartialOrderDag.AddVertex[V, E, G](v, left, right)))
    } else {
      (this, None)
    }

  def remove(v: V): PartialOrderDag.UpdateResult[V, E, G] =
    if (value.contains(v) && !graphLike.isSentinel(v)) {
      val nextVertices = vertices - v
      val next = new PartialOrderDag[V, E, G](nextVertices, edges)
      (next, Some(PartialOrderDag.RemoveVertex[V, E, G](v)))
    } else {
      (this, None)
    }

  def run(operation: PartialOrderDag.AddVertex[V, E, G]): (PartialOrderDag[V, E, G], Option[Update[V, E, G]]) =
    add(operation.v, operation.left, operation.right)

  def run(operation: PartialOrderDag.RemoveVertex[V, E, G]): (PartialOrderDag[V, E, G], Option[Update[V, E, G]]) =
    remove(operation.v)
}

object PartialOrderDag {
  type UpdateResult[V, E[X] <: DiEdgeLikeIn[X], G <: Graph[V, E]] = (PartialOrderDag[V, E, G], Option[Update[V, E, G]])

  sealed trait Update[V, E[X] <: DiEdgeLikeIn[X], G <: Graph[V, E]]
  case class AddVertex[V, E[X] <: DiEdgeLikeIn[X], G <: Graph[V, E]](v: V, left: V, right: V) extends Update[V, E, G]
  case class RemoveVertex[V, E[X] <: DiEdgeLikeIn[X], G <: Graph[V, E]](v: V) extends Update[V, E, G]
}
