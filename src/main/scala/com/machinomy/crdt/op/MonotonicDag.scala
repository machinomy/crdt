package com.machinomy.crdt.op

import GraphTypes._

/*
case class MonotonicDag[V, E, G](graph: GraphProxy[V, E, G])(implicit toProxy: G => GraphProxy[V, E, G]) {
  def value = graph

  def contains(v: V): Boolean = graph.contains(v)

  def existsPath(from: V, to: V, effective: G) = graph.existsPath(from, to)

  def add(e: E) = {
    val from = graph.fromVertex(e)
    val to = graph.toVertex(e)
    val containsSource: Boolean = contains(from)
    val containsDestination: Boolean = contains(to)
    val effective = graph.addEdge(e)
    if (containsSource && containsDestination && existsPath(from, to, effective)) {
      (MonotonicDag(effective), Some(MonotonicDag.AddEdge[V, E, G](e)))
    } else {
      (this, None)
    }
  }
}

object MonotonicDag {
  type UpdateResult[V, E, G] = (MonotonicDag[V, E, G], Option[Update[V, E, G]])

  sealed trait Update[V, E, G]
  case class AddEdge[V, E, G](e: E) extends Update[V, E, G]
}
*/
