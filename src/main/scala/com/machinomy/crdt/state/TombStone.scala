package com.machinomy.crdt.state

import com.github.nscala_time.time.Imports._

trait TombStone[A] {
  def next: A
  def ordering: Ordering[A]
  def zero: A
}

object TombStone {
  implicit val dateTime = new TombStone[DateTime] {
    override def next: DateTime = DateTime.now()
    override def ordering: Ordering[DateTime] = implicitly[Ordering[DateTime]]
    override def zero: DateTime = new DateTime(0)
  }
}
