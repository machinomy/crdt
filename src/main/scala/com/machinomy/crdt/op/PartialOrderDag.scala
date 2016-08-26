/*
 * Copyright 2016 Machinomy
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
