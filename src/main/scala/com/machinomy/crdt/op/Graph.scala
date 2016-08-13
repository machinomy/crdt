package com.machinomy.crdt.op

import scala.language.higherKinds

object Graph {
  trait VertexLike[A]
  trait SentinelLike[A] extends VertexLike[A]
  trait EdgeLike[A, V <: VertexLike[A]] {
    def u: V
    def v: V
  }
  trait GraphLike[A, V <: VertexLike[A]]
}
