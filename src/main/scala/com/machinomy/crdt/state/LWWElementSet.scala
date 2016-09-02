package com.machinomy.crdt.state

import cats._
import cats.syntax.all._
import com.machinomy.crdt.state.LWWElementSet.Moves

/** Last-Writer-Wins Set attaches a timestamp to an element addition or removal. An element is present iff addition timestamp is bigger than that of removal.
  * Concurrent addition and removal is resolved using `Bias`: either addition or removal always wins.
  *
  * @tparam E Element contained
  * @tparam T Timestamp
  * @tparam B Bias
  * @see [[com.machinomy.crdt.state.LWWElementSet.monoid]] Behaves like a [[cats.Monoid]]
  * @see [[com.machinomy.crdt.state.LWWElementSet.partialOrder]] Behaves like a [[cats.PartialOrder]]
  * @see [[com.machinomy.crdt.state.Bias]] Either addition or removal always wins
  * @see [[com.machinomy.crdt.state.LWWElementSet.Moves]] Container for removal/addition timestamps
  * @see Shapiro, M., Preguiça, N., Baquero, C., & Zawirski, M. (2011).
  *      Conflict-free replicated data types.
  *      In Proceedings of the 13th international conference on Stabilization, safety, and security of distributed systems (pp. 386–400).
  *      Grenoble, France: Springer-Verlag.
  *      Retrieved from [[http://dl.acm.org/citation.cfm?id=2050642]]
  */
case class LWWElementSet[E, T: PartialOrder, B: Bias](state: Map[E, LWWElementSet.Moves[T]])(implicit tombStone: TombStone[T], movesMonoid: Monoid[Moves[T]]) extends Convergent[E, Set[E]] {
  type Self = LWWElementSet[E, T, B]

  /** Add `element` to the set.
    *
    * @return Updated LWWElementSet
    */
  def +(element: E): Self = {
    val stone = tombStone.next
    this + (element, stone)
  }

  /** Add `element` to the set stamped by `stone`.
    *
    * @return Updated LWWElementSet
    */
  def +(element: E, stone: T): Self = {
    val moves = state.getOrElse(element, movesMonoid.empty).copy(addition = Some(stone))
    new LWWElementSet(state.updated(element, moves))
  }

  /** Add `pair._1` to the set stamped by `pair._2`.
    *
    * @return Updated LWWElementSet
    */
  def +(pair: (E, T)): Self = this + (pair._1, pair._2)

  /** Remove `element` from the set.
    *
    * @return Updated LWWElementSet
    */
  def -(element: E): Self = {
    val stone = tombStone.next
    this - (element, stone)
  }

  /** Remove `element` from the set. Removal is stamped by `stone`.
    *
    * @return Updated LWWElementSet
    */
  def -(element: E, stone: T): Self = {
    val moves = state.getOrElse(element, movesMonoid.empty).copy(removal = Some(stone))
    new LWWElementSet(state.updated(element, moves))
  }

  /** Remove `pair._1` from the set. Removal is stamped by `pair._2`.
    *
    * @return Updated LWWElementSet
    */
  def -(pair: (E, T)): Self = this - (pair._1, pair._2)

  /** @return Value of the set.
    */
  override def value: Set[E] = state.foldLeft(Set.empty[E]) { case (memo, (element, moves)) =>
    elementByMoves(element, moves) match {
      case Some(e) => memo + e
      case None => memo
    }
  }

  /** Decide if element is worth keeping in a `value` set.
    */
  protected def elementByMoves(element: E, moves: Moves[T])(implicit bias: Bias[B]): Option[E] =
    moves match {
      case Moves(Some(addition), Some(removal)) =>
        bias.apply(element, addition, removal)
      case Moves(Some(addition), None) =>
        Some(element)
      case _ =>
        None
    }
}

object LWWElementSet {

  /** Record addition and removal tags for [[ORSet]].
    *
    * @tparam T Addition and removal tag
    * @see [[com.machinomy.crdt.state.LWWElementSet.Moves.monoid]] Behaves like a [[cats.Monoid]]
    * @see [[com.machinomy.crdt.state.LWWElementSet.Moves.partialOrder]] Behaves like a [[cats.PartialOrder]]
    */
  case class Moves[T](addition: Option[T] = None, removal: Option[T] = None)

  object Moves {
    /** Implements [[cats.Monoid]] type class for [[Moves]].
      *
      * @tparam T Unique tag
      */
    implicit def monoid[T](implicit order: PartialOrder[T]) = new Monoid[Moves[T]] {
      override def empty: Moves[T] = new Moves[T](None, None)

      override def combine(x: Moves[T], y: Moves[T]): Moves[T] = {
        def next(optA: Option[T], optB: Option[T]) = (optA, optB) match {
          case (Some(a), Some(b)) if order.gteqv(a, b) => Some(a)
          case (Some(a), Some(b)) => Some(b)
          case (Some(a), None) => Some(a)
          case (None, Some(b)) => Some(b)
          case _ => None
        }

        val addition = next(x.addition, y.addition)
        val removal = next(x.removal, y.removal)
        new Moves(addition, removal)
      }
    }

    /** Implements [[cats.PartialOrder]] type class for [[Moves]].
      *
      * @tparam T Unique tag
      */
    implicit def partialOrder[T](implicit order: PartialOrder[T]) = PartialOrder.byLteqv[Moves[T]] { (x, y) =>
      def lteqv(optA: Option[T], optB: Option[T]): Boolean = (optA, optB) match {
        case (Some(a), Some(b)) if order.lteqv(a, b) => true
        case (Some(a), None) => true
        case (None, Some(b)) => false
        case (_, _) => true
      }
      lteqv(x.addition, y.addition) && lteqv(x.removal, y.removal)
    }
  }

  /** Implements [[cats.Monoid]] type class for [[LWWElementSet]].
    *
    * @tparam E Contained element
    * @tparam T Unique tag
    * @tparam B Bias
    */
  implicit def monoid[E, T: PartialOrder, B: Bias](implicit tombStone: TombStone[T]) = new Monoid[LWWElementSet[E, T, B]] {
    override def empty: LWWElementSet[E, T, B] = new LWWElementSet(Map.empty[E, LWWElementSet.Moves[T]])
    override def combine(x: LWWElementSet[E, T, B], y: LWWElementSet[E, T, B]): LWWElementSet[E, T, B] = {
      val elements = x.state.keySet ++ y.state.keySet
      val state = elements.foldLeft(Map.empty[E, Moves[T]]) { case (memo, element) =>
        val xMoves = x.state.getOrElse(element, Moves[T]())
        val yMoves = y.state.getOrElse(element, Moves[T]())
        val moves = xMoves |+| yMoves
        memo.updated(element, moves)
      }
      new LWWElementSet(state)
    }
  }

  /** Implements [[cats.PartialOrder]] type class for [[LWWElementSet]].
    *
    * @tparam E Contained element
    * @tparam T Unique tag
    * @tparam B Bias
    */
  implicit def partialOrder[E, T, B: Bias](implicit partialOrder: PartialOrder[Moves[T]], movesMonoid: Monoid[Moves[T]]) = PartialOrder.byLteqv[LWWElementSet[E, T, B]] { (x, y) =>
    x.state.keySet.forall { (element) =>
      val xMoves = x.state.getOrElse(element, Monoid[Moves[T]].empty)
      val yMoves = y.state.getOrElse(element, Monoid[Moves[T]].empty)
      partialOrder.lteqv(xMoves, yMoves)
    }
  }
}
