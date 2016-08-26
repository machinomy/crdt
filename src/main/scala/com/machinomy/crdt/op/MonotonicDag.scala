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

import scala.language.higherKinds
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._

case class MonotonicDag[V, E[X] <: DiEdgeLikeIn[X], G <: Graph[V, E]](graph: G)(implicit graphLike: DiGraphLike[V, E, G]) {
  def value = graph

  def add(e: E[V] with OuterEdge[V, E]): MonotonicDag.UpdateResult[V, E, G] = {
    val from = graphLike.fromVertex(graph, e)
    val to = graphLike.toVertex(value, e)
    val containsSource = graphLike.contains(value, from)
    val containsDestination = graphLike.contains(value, to)
    val effective = graphLike.addEdge(value, e)
    if (containsSource && containsDestination && graphLike.existsPath(effective, from, to)) {
      (MonotonicDag[V, E, G](effective), Some(MonotonicDag.AddEdge[V, E, G](e)))
    } else {
      (this, None)
    }
  }

  def add(v: V, left: V, right: V): MonotonicDag.UpdateResult[V, E, G] = {
    val absentV = !graphLike.contains(graph, v)
    val presentLeft = graphLike.contains(graph, left)
    val presentRight = graphLike.contains(graph, right)
    val existsPath = graphLike.existsPath(graph, left, right)
    if (absentV && presentLeft && presentRight && existsPath) {
      val g1 = graphLike.addVertex(graph, v)
      val g2 = graphLike.addEdge(g1, graphLike.buildEdge(left, v))
      val g3 = graphLike.addEdge(g2, graphLike.buildEdge(v, right))
      val next = MonotonicDag[V, E, G](g3)
      (next, Some(MonotonicDag.AddVertex[V, E, G](v, left, right)))
    } else {
      (this, None)
    }
  }
}

object MonotonicDag {
  type UpdateResult[V, E[X] <: DiEdgeLikeIn[X], G <: Graph[V, E]] = (MonotonicDag[V, E, G], Option[Update[V, E, G]])

  sealed trait Update[V, E[X] <: DiEdgeLikeIn[X], G <: Graph[V, E]]
  case class AddEdge[V, E[X] <: DiEdgeLikeIn[X], G <: Graph[V, E]](e: E[V]) extends Update[V, E, G]
  case class AddVertex[V, E[X] <: DiEdgeLikeIn[X], G <: Graph[V, E]](v: V, left: V, right: V) extends Update[V, E, G]
}
