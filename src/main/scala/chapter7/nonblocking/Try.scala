package chapter7.nonblocking

import scala.util.control.NonFatal

trait Try[+T] {
  def get: T
}

object Try {
  /** Constructs a `Try` using the by-name parameter.  This
   * method will ensure any non-fatal exception is caught and a
   * `Failure` object is returned.
   */
  def apply[T](r: => T): Try[T] =
    try Success(r) catch {
      case NonFatal(e) => Failure(e)
    }

}

case class Failure[+T](exception: Throwable) extends Try[T] {
  def get: T = throw exception
}
case class Success[+T](value: T) extends Try[T] {
  def get: T = value
}
