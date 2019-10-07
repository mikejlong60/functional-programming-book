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

  def concatenate[A](as: IndexedSeq[A])(m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def lFoldMap[A, B](as: IndexedSeq[A])(m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def  foldLeft[A, B](as: IndexedSeq[A])(m: Monoid[B])(f: A => B): B = Monoid.lFoldMap(as)(m)(f)

  def rFoldMap[A, B](as: IndexedSeq[A])(m: Monoid[B])(f: A => B): B = as.foldRight(m.zero)((a, b) => m.op(b, f(a)))

  def  foldRight[A, B](as: IndexedSeq[A])(m: Monoid[B])(f: A => B): B = Monoid.rFoldMap(as)(m)(f)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.size == 0) m.zero
    else if (v.size == 1) f(v(0))
    else {
      val (half1, half2) = v.splitAt(v.size / 2)
      val fs = foldMapV(half1, m)(f)
      val sn = foldMapV(half2, m)(f)
      m.op(fs, sn)
    }
  }

}


