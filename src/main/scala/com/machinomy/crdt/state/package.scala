package com.machinomy.crdt

import cats._
import com.github.nscala_time.time.Imports._

package object state {
  implicit object DateTimeOrder extends Order[DateTime] {
    override def compare(x: DateTime, y: DateTime): Int = {
      x.compare(y)
    }
  }
}
