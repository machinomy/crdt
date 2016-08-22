package com.machinomy.crdt.op

import scala.language.implicitConversions
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.{DiEdgeLikeIn, EdgeLikeIn}

object GraphTypes {
  trait GraphProxy {
    type V
    type E
    type G
  }

  trait GraphLike[G <: GraphProxy] {
    def contains(graph: G#G, vertex: G#V): Boolean
    def existsPath(graph: G#G, from: G#V, to: G#V): Boolean
    def addEdge(graph: G#G, edge: G#E): G#G
    def addVertex(graph: G#G, v: G#V): G#G
    def sourceVertex(graph: G#G, e: G#E): G#V
    def destinationVertex(graph: G#G, e: G#E): G#V
  }
}
