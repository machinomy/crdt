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

trait DiGraphLike[V, E[X] <: DiEdgeLikeIn[X], G <: Graph[V, E]] {
  def contains(graph: G, vertex: V): Boolean
  def addEdge(graph: G, edge: E[V] with OuterEdge[V, E]): G
  def addVertex(graph: G, vertex: V): G
  def fromVertex(graph: G, edge: E[V] with OuterEdge[V, E]): V
  def toVertex(graph: G, edge: E[V] with OuterEdge[V, E]): V
  def path(graph: G, from: V, to: V): Option[G#Path]
  def existsPath(graph: G, from: V, to: V): Boolean
  def buildEdge(from: V, to: V): E[V] with OuterEdge[V, E] // @todo CanBuildEdge
  def buildGraph(vertices: Set[V], edges: Set[E[V]]): G // @todo CanBuildGraph
  def isSentinel(v: V): Boolean // @todo CanDetectSentinel
}

object DiGraphLike {
  implicit object IntDiGraphLike extends DiGraphLike[Int, DiEdge, Graph[Int, DiEdge]] {
    override def contains(graph: Graph[Int, DiEdge], vertex: Int): Boolean =
      graph.contains(vertex)
    override def addEdge(graph: Graph[Int, DiEdge], edge: DiEdge[Int] with OuterEdge[Int, DiEdge]): Graph[Int, DiEdge] =
      graph + edge
    override def addVertex(graph: Graph[Int, DiEdge], vertex: Int): Graph[Int, DiEdge] =
      graph + vertex
    override def fromVertex(graph: Graph[Int, DiEdge], edge: DiEdge[Int] with OuterEdge[Int, DiEdge]): Int =
      edge.from
    override def toVertex(graph: Graph[Int, DiEdge], edge: DiEdge[Int] with OuterEdge[Int, DiEdge]): Int =
      edge.to
    override def path(graph: Graph[Int, DiEdge], from: Int, to: Int): Option[graph.Path] =
      for {
        fromVertex <- graph.find(from)
        toVertex <- graph.find(to)
        path <- fromVertex.pathTo(toVertex)
      } yield path
    override def existsPath(graph: Graph[Int, DiEdge], from: Int, to: Int): Boolean =
      path(graph, from, to).isDefined
    override def buildEdge(from: Int, to: Int): DiEdge[Int] =
      from ~> to
    override def buildGraph(vertices: Set[Int], edges: Set[DiEdge[Int]]): Graph[Int, DiEdge] = Graph.from(vertices, edges)
    override def isSentinel(v: Int): Boolean = v == 1 || v == 100
  }
}
