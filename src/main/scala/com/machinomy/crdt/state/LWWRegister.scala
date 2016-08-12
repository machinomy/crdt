package com.machinomy.crdt.state

import cats.kernel.Semilattice

case class LWWRegister[E, T: TombStone : Ordering](value: E, timestamp: T) extends Convergent[E, E] {
  override type Self = LWWRegister[E, T]
}

object LWWRegister {
  def apply[E, T: TombStone : Ordering](value: E): LWWRegister[E, T] = LWWRegister[E, T](value, implicitly[TombStone[T]].next)

  implicit def semilattice[E, T: TombStone : Ordering] = new Semilattice[LWWRegister[E, T]] {
    override def combine(x: LWWRegister[E, T], y: LWWRegister[E, T]): LWWRegister[E, T] = {
      val ordering = implicitly[Ordering[T]]
      if (ordering.gt(x.timestamp, y.timestamp)) {
        x
      } else {
        y
      }
    }
  }
}
