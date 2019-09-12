package chapter4

import scala.{List => _, Option => _, Either => _, _}
import chapter3.List
import chapter3.Cons
import chapter3.Nil

case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

sealed trait Option[+A] { // +A means that A is covariant or positive.  If I left out the + then A would be invariant.   The + sign means that the
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse (None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  //Book says no pattern matching
  def orElse2[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case Some(a) => Some(a)
      case _ => ob
    }

  def ccc[B >: A](x:B ): Option[B] =  Some(x): Option[B]  //B >: A is the notation for an f-bounded type.
  def orElse32[B >: A](ob: => Option[B]): Option[B] = map(x => Some(x):Option[B]) getOrElse ob
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(ccc)  getOrElse ob

  //Book says no pattern matching
  def filter2(f: A => Boolean): Option[A] = this match {
    case Some(a) => {
      if (f(a)) Some(a)
      else None
    }
    case _ => None
  }

  def filter(f: A => Boolean): Option[A] = {
    def g: (A => Option[A]) = (a: A) => {
      if (f(a)) Some(a)
      else None
    }

    flatMap(g)
  }
}

object Option {
  def lift[A, B](f: A => B): Option[A]  => Option[B]  ={
    //Implicitely the _ gets converted to my Option type.  The compiler derives this information from the closest resolution to the function that follows, which in this case is map.
    _  map(f)
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C] = a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Cons(maybeX, Nil) => maybeX.map(x => Cons(x, Nil))
    case Cons(maybeX,  xs) => maybeX.flatMap(x => sequence(xs).map(xxs => Cons(x, xxs)))
    case Nil => None
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    //sequence(a.map(a => f(a)))

  a match {
    case Cons(h, t) =>  map2(f(h), traverse(t)(f))((a, b) => Cons(a, b))
    case _ => Some(Nil)
  }

  
}
