package com.machinomy.crdt.state

case class LWWRegister[E, T: TombStone : Ordering](value: E, timestamp: T) extends Convergent[E, E] {
  override type Self = LWWRegister[E, T]

  override def merge(that: Self): Self = {
    val ordering = implicitly[Ordering[T]]
    if (ordering.gt(that.timestamp, this.timestamp)) {
      that
    } else {
      this
    }
  }
}

object LWWRegister {
  def apply[E, T: TombStone : Ordering](value: E): LWWRegister[E, T] = LWWRegister[E, T](value, implicitly[TombStone[T]].next)
}