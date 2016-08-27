package com.machinomy.crdt.state

import cats.{PartialOrder => CatPartialOrder}

/** Constructor of [[cats.PartialOrder]] instances based on `lteqv` relation.
 *
  * @see [[PartialOrder.byLteqv]]
  */
object PartialOrder {
  /** [[cats.PartialOrder]] based on `lteqv` relation.
    *
    * @param f lteqv relation
    */
  def byLteqv[A](f: (A,A) => Boolean) = new CatPartialOrder[A] {
    override def partialCompare(x: A, y: A): Double =
      (lteqv(x, y), lteqv(y, x)) match {
        case (true, true) => 0
        case (false, true) => 1
        case (true, false) => -1
        case (false, false) => Double.NaN
      }

    override def lteqv(x: A, y: A): Boolean = f(x, y)
  }
}
