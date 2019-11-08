package chapter10

import chapter5.Stream._
import chapter5.Cons

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B 
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B = foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  def  toList[A](as: F[A]): List[A] = foldRight(as)(List[A]())(_ :: _)
}

object FoldableInstances {
  def list: Foldable[List]= new Foldable[List]  {
    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldLeft(as.reverse)(z)((b, a) => f(a, b))
  }

  def seq: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = foldLeft(as.reverse)(z)((b, a) => f(a, b))
  }

  def stream: Foldable[chapter5.Stream] = new Foldable[chapter5.Stream] {
    def foldLeft[A, B](as: chapter5.Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)((a, b) => f(a, b))
    def foldRight[A, B](as: chapter5.Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)((a, b) => f(a, b))
  }
}

