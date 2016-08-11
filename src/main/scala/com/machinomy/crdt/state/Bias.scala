package com.machinomy.crdt.state

trait Bias[B] {
  def apply[E, T: TombStone](element: E, addition: T, removal: T): Option[E]
}

object Bias {
  sealed trait Direction
  case class AdditionWins() extends Direction
  case class RemovalWins() extends Direction

  implicit val additionBias = new Bias[AdditionWins] {
    override def apply[E, T: TombStone](element: E, add: T, remove: T): Option[E] = {
      val ordering = implicitly[TombStone[T]].ordering
      if (ordering.gteq(add, remove)) {
        Some(element)
      } else {
        None
      }
    }
  }

  implicit val removalBias = new Bias[RemovalWins] {
    override def apply[E, T: TombStone](element: E, add: T, remove: T): Option[E] = {
      val ordering = implicitly[TombStone[T]].ordering
      if (ordering.gteq(remove, add)) {
        None
      } else {
        Some(element)
      }
    }
  }
}
