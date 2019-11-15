package chapter10

import chapter5.Stream._
import chapter5.Cons

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B 
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B = foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List.empty[A])((a, b) => a :: b)
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
    def foldLeft[A,B](as: chapter3.Tree[A])(z: B)(f: (B,A) => B):B = as match {
      case chapter3.Leaf(a) => f(z, a)
      case chapter3.Branch(la, ra) => foldLeft(ra)(foldLeft(la)(z)(f))(f)
      case chapter3.NilNode => z
    }
    
    def foldRight[A,B](as: chapter3.Tree[A])(z: B)(f: (A,B) => B):B =  as match {
      case chapter3.Leaf(a) => f(a, z)
      case chapter3.Branch(la, ra) => foldRight(la)(foldRight(ra)(z)(f))(f) 
      case chapter3.NilNode => z
    }

    override def foldMap[A,B](as: chapter3.Tree[A])(f: A => B)(mb: Monoid[B]): B = chapter3.Tree.fold(as)(f)(mb.op)(() => mb.zero)
  }

  def option: Foldable[Option] = new Foldable[Option] {
    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B):B = as match {
      case Some(a) => f(z, a)
      case None => z
    }
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B):B = as match {
      case Some(a) => f(a, z)
      case None => z
    }

  }
}

