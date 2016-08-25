/*
 * Copyright 2016 Sergey Ukustov, Konstantin Makarychev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.machinomy.crdt.op

import com.machinomy.crdt.state.TombStone

case class ORSet[E, T: TombStone](state: Map[E, Set[T]]) {
  lazy val value: Set[E] = state.keySet

  def add(e: E): ORSet.UpdateResult[E, T] = {
    val t: T = implicitly[TombStone[T]].next
    val nextTombstones = state.getOrElse(e, Set.empty[T]) + t
    val next = ORSet(state + (e -> nextTombstones))
    (next, Some(ORSet.Add(e, t)))
  }

  def contains(e: E): Boolean = value.contains(e)

  def remove(e: E): ORSet.UpdateResult[E, T] = {
    val tombstones = state.getOrElse(e, Set.empty[T])
    val next = ORSet(state - e)
    (next, Some(ORSet.Remove(e, tombstones)))
  }

  def run(operation: ORSet.Add[E, T]): ORSet.UpdateResult[E, T] =
    if (contains(operation.e)) {
      (this, None)
    } else {
      val tombstones = state.getOrElse(operation.e, Set.empty[T])
      val nextState = state.updated(operation.e, tombstones)
      val next = ORSet(nextState)
      (next, Some(operation))
    }

  def run(operation: ORSet.Remove[E, T]): ORSet.UpdateResult[E, T] =
    if (contains(operation.e)) {
      val existingTombstones = state.getOrElse(operation.e, Set.empty[T])
      val nextTombstones = existingTombstones -- operation.tombstones
      val nextState =
        if (nextTombstones.nonEmpty) {
          state.updated(operation.e, nextTombstones)
        } else {
          state - operation.e
        }
      (new ORSet(nextState), Some(operation))
    } else {
      (this, None)
    }

}

object ORSet {
  type UpdateResult[E, T] = (ORSet[E, T], Option[ORSet.Update[E]])

  def apply[E, T: TombStone](): ORSet[E, T] = ORSet(Map.empty[E, Set[T]])

  sealed trait Update[E]
  case class Add[E, T: TombStone](e: E, t: T) extends Update[E]
  case class Remove[E, T: TombStone](e: E, tombstones: Set[T]) extends Update[E]
}
