package com.machinomy.crdt.op

import com.machinomy.crdt.op.PartialOrderDag.Update
import com.machinomy.crdt.state.TPSet

import scala.language.higherKinds
import scalax.collection.Graph
import scalax.collection.GraphPredef._

/**
  * Add-Remove Partial Order
  */
case class PartialOrderDag[V, E[X] <: DiEdgeLikeIn[X]](vertices: TPSet[V], edges: Set[E[V]])(implicit graphLike: DiGraphLike[V, E, Graph[V, E]]) {
  lazy val value: Graph[V, E] = graphLike.buildGraph(vertices.value, edges)

  def add(v: V, left: V, right: V): PartialOrderDag.UpdateResult[V, E] = {
    if (!value.contains(v) && graphLike.existsPath(value, left, right)) {
      val nextVertices = vertices + v
      val leftEdge = graphLike.buildEdge(left, v).edge
      val rightEdge = graphLike.buildEdge(v, right).edge
      val nextEdges = edges + leftEdge + rightEdge
      val next = new PartialOrderDag[V, E](nextVertices, nextEdges)
      (next, Some(PartialOrderDag.AddVertex[V, E](v, left, right)))
    } else {
      (this, None)
    }
  }

  // @todo This does not work.
  def remove(v: V): PartialOrderDag.UpdateResult[V, E] =
    if (value.contains(v) && !graphLike.isSentinel(v)) {
      val nextVertices = vertices - v
      val next = new PartialOrderDag[V, E](nextVertices, edges)
      (next, Some(PartialOrderDag.RemoveVertex[V, E](v)))
    } else {
      (this, None)
    }

  def run(operation: PartialOrderDag.AddVertex[V, E]): (PartialOrderDag[V, E], Option[Update[V, E]]) =
    add(operation.v, operation.left, operation.right)

  def run(operation: PartialOrderDag.RemoveVertex[V, E]): (PartialOrderDag[V, E], Option[Update[V, E]]) =
    remove(operation.v)
}

object PartialOrderDag {
  type UpdateResult[V, E[X] <: DiEdgeLikeIn[X]] = (PartialOrderDag[V, E], Option[Update[V, E]])

  sealed trait Update[V, E[X] <: DiEdgeLikeIn[X]]

  case class AddVertex[V, E[X] <: DiEdgeLikeIn[X]](v: V, left: V, right: V) extends Update[V, E]

  case class RemoveVertex[V, E[X] <: DiEdgeLikeIn[X]](v: V) extends Update[V, E]

}
