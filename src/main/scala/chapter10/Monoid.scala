package chapter10

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A

  def associativeLaw(x: A, y: A, z: A): Boolean = op(op(x, y), z) == op(x, op(y, z))
  def zeroLaw(x: A): Boolean = op(x, zero) == x


}

object Monoid {

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A]  {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def concatenate[A](as: List[A])(m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def lFoldMap[A, B](as: List[A])(m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def  foldLeft[A, B](as: List[A])(m: Monoid[B])(f: A => B): B = Monoid.lFoldMap(as)(m)(f)

  def rFoldMap[A, B](as: List[A])(m: Monoid[B])(f: A => B): B = as.foldRight(m.zero)((a, b) => m.op(b, f(a)))

  def  foldRight[A, B](as: List[A])(m: Monoid[B])(f: A => B): B = Monoid.rFoldMap(as)(m)(f)

}
