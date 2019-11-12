package chapter10

import chapter5.Stream._
import chapter5.Cons

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B 
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B = foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

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

  def tree: Foldable[chapter3.Tree] = new Foldable[chapter3.Tree] {
    def foldLeft[A,B](as: chapter3.Tree[A])(z: B)(f: (B,A) => B):B = {
      def curry[A, B](f: (B, A) => B): B => (A => B) = a => b =>  f(a, b)
      def partial1[A, B](b: B, f: (B, A) => B): A => B = (a: A) => {
        val c = curry(f)
        val g = c(b)(_)
        g(a)
      }
      val ff:(A=>B) = partial1(z, f)
      val g:((B,B) => B) = ???
      val h: () => B = () => z
      chapter3.Tree.fold(as)(ff)(g)(h)
    }
    def foldRight[A,B](as: chapter3.Tree[A])(z: B)(f: (A,B) => B):B = {
      def curry[A, B](f: (A,B) => B): B => (A => B) = b => a =>  f(a,b)
      def partial1[A, B](b: B, f: (A, B) => B): A => B = (a: A) => {
        val c = curry(f)
        val g = c(b)(_)
        g(a)
      }
      val ff:(A=>B) = partial1(z, f)
      val g:((B,B) => B) = ???
      val h: () => B = () => z
      chapter3.Tree.fold(as)(ff)(g)(h)
    }

    def foldMap[A,B](as: chapter3.Tree[A])(f: A => B)(mb: Monoid[B]): B = chapter3.Tree.fold(as)(f)(mb.op)(() => mb.zero)
  }
}

