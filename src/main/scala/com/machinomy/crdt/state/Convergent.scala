package com.machinomy.crdt.state

trait Convergent[Element, Value] {
  def value: Value
}
