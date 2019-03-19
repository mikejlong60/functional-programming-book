package chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.00
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil //Choose to return Nil instead of throw an exception
    case Cons(x, xs) => xs
  }

  def head[A](l: List[A]): A =
    l match {
      case Nil => throw new Exception("head of empty list")
      case Cons(h, t) => h
    }

  def setHead[A](newHead: A, xs: List[A]): List[A] = xs match {
    case Nil => Cons(newHead, Nil)
    case Cons(x, xs) => Cons(newHead, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
  }

  //Repeately removes from the head of the list as long as the head matches the predicate.  But stops once it has reached a false predicate
  @annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if (f(x)) => dropWhile(xs)(f)
    case _ => l
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    case _ => z
  }

  //Will blow stack for large lists. See foldRightTailRec for stack-safe version
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((z, x) => Cons(x, z))

  def foldRightTailRec[A, B](as: List[A], z: B)(f: (A, B) => B): B = { //Is isomorphic to foldRight
    val reversed = reverse(as)
    foldLeft(reversed, z)((b, a) => f(a, b))
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = foldRightTailRec(l1, l2)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])((a, b) => append(a, b)) //Can be foldRightTailRec

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((x, z) => Cons(f(x), z))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((x, z) => if (f(x)) Cons(x, z) else z)

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else List() )

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def zipFold[A, B, C](l1: List[A], l2: List[B], accum: List[C])(f: (A, B) => C): List[C] = {
     (l1, l2) match {
       case (Cons(x, xs), Cons(y, ys)) => zipFold(xs, ys, Cons(f(x, y), accum))(f)
       case _ => reverse(accum)
     }
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = zipFold(l1, l2, Nil: List[C])(f)

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    @tailrec
    def internal(sup: List[A], sub: List[A]): (List[A], List[A]) = (sup, sub) match {
      case (Cons(x, xs), Cons(y, ys))  if (x == y) => internal(xs, ys)
      case (Cons(x, xs), Cons(y, ys))  if (x != y) => internal(xs, sub)
      case (xs @ _, ys @ _) => (xs, ys)
    }

    internal(sup, sub) match {
      case (Cons(x, xs), Nil) => true
      case (Nil, Cons(y, ys)) => false
      case _ => true
    }
  }
}
