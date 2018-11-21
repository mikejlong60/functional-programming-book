package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil//Choose to return Nil instead of throw an exception
    case Cons(x, xs) => xs
  }

  def setHead[A](newHead: A, xs: List[A]): List[A] = xs match {
    case Nil => Cons(newHead, Nil)
    case Cons(x, xs) => Cons(newHead, xs)
  }
}