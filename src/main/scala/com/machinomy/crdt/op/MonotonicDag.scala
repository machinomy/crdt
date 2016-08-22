package com.machinomy.crdt.op

import GraphProxy._
/*
case class MonotonicDag[G <: GraphProxy](graph: G)(implicit graphLike: GraphLikeA[G]) {
  def value = graph.graph

  def add(e: G#E) = {
    val from = graphLike.fromVertex(value, e)
    val to = graphLike.toVertex(value, e)
    val containsSource = graphLike.contains(value, from)
    val containsDestination = graphLike.contains(value, to)
    val effective = graphLike.addEdge(value, e)
    if (containsSource && containsDestination && graphLike.existsPath(effective, from, to)) {
      val nextGraphProxy = graphLike.toProxy(effective)
      (MonotonicDag(nextGraphProxy), Some(MonotonicDag.AddEdge[G](e)))
    } else {
      (this, None)
    }
  }
}

object MonotonicDag {
  type UpdateResult[G <: GraphProxy] = (MonotonicDag[G], Option[Update[G]])

  sealed trait Update[G <: GraphProxy]
  case class AddEdge[G <: GraphProxy](e: G#E) extends Update[G]
}
*/
