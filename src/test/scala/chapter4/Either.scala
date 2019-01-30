package chapter4

import scala.{List => _, Option => _, Either => _, _}
import chapter3.List
import chapter3.Cons
import chapter3.Nil


case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
sealed trait Either[+E, +A] { // + means that the type is covariant or positive.  - means that the type is contravariant or negative.  The absence of either + or - means the type is invariant.  + means that the type must be a subtype of the main type.  - means that the type must be a supertype of the main type. For example if the main type were Numeric it would type check for you to construct a Right[Short] or Right[Int].  But if the main type was an Int it would not type check to conctruct a Right[Long].  See the B >: A example below for an example of contravariant.
  
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
 
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }
  
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }
  
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) : Either[EE, C] = this.flatMap(a => b.map(b => f(a, b)))

}

object Either {
    def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {case e: Exception => Left(e)}


}
