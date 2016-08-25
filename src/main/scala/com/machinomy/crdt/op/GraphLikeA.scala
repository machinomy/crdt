/*
 * Copyright 2016 Sergey Ukustov, Konstantin Makarychev
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

object GraphLikeA {
  trait VertexLike[A]
  trait SentinelLike[A] extends VertexLike[A]
  trait EdgeLike[A, V <: VertexLike[A]] {
    def u: V
    def v: V
  }
  trait GraphLike[A, V <: VertexLike[A], E <: EdgeLike[A, V]] {
    def vertices: Set[V]
    def edges: Set[E]
    def existsPath(u: V, v: V): Boolean
    def add(e: E): GraphLike[A, V, E]
    def add(v: V): GraphLike[A, V, E]
  }
  trait CanBuildEdge[A, V <: VertexLike[A], E <: EdgeLike[A, V]] {
    def buildEdge(u: V, v: V): E
  }
  trait CanBuildGraph[A, V <: VertexLike[A], E <: EdgeLike[A, V]] {
    def buildGraph(vertices: Set[V], edges: Set[E]): GraphLike[A, V, E]
  }
}
