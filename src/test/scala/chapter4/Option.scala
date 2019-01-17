package chapter4

import scala.{Option => _, Either => _, _}

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
