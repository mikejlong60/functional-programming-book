package chapter10

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A

  def associativeLaw(x: A, y: A, z: A): Boolean = op(op(x, y), z) == op(x, op(y, z))
  def zeroLaw(x: A): Boolean = op(x, zero) == x
}
