package com.machinomy.crdt.state

trait Convergent[Element, Value] {
  type Self <: Convergent[Element, Value]
  def merge(that: Self): Self
  def value: Value
}
