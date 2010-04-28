package org.freetle.util

/**
 * Helper class to memoize1 a one parameter function.
 */
class Memoize1[-T, +R](f: T => R) extends (T => R) {
  import scala.collection.mutable
  private[this] val vals = mutable.Map.empty[T, R]

  def apply(x: T): R = {
    if (vals.contains(x)) {
      vals(x)
    }
    else {
      val y = f(x)
      vals + ((x, y))
      y
    }
  }
}

object Memoize1 {
  def apply[T, R](f: T => R) = new Memoize1(f)
}