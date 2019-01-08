package chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case object NilNode extends Tree[Nothing]

object Tree {
  import scala.math.max


  //Old implementations, not in terms of Fold
//  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
//    case Leaf(a) => Leaf(f(a))
//    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
//  }
//
//  def maximum(t: Tree[Int]): Int = t match {
//    case Leaf(n) => n
//    case Branch(l,r) => maximum(l) max maximum(r)
//  }
//
//  def depth[A](t: Tree[A]): Int = t match {
//    case Leaf(_) => 0
//    case Branch(l,r) => 1 + (depth(l) max depth(r))
//  }
//
//  def size[A](tree: Tree[A]): Int = tree match {
//    case Leaf(_) => 1
//    case Branch(l, r) => 1 + size(l) + size(r)
//  }

  //Note the trick here. I can generally replace pattern matching with one function per pattern.

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B)(h: () => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g)(h), fold(r)(f)(g)(h))
    case NilNode => h()
  }

  def size[A](t: Tree[A]): Int = fold(t)(a => 1)((b1, b2) => 1 + b1 + b2)(() => 0)

  def maximum(t: Tree[Int]): Int = fold(t)(a => a)((b1, b2) => b1 max b2)(() => Int.MinValue)

  def depth[A](t: Tree[A]): Int = fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))(() => 0)

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))(() => NilNode: Tree[B])
}
