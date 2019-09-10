package chapter11

import scala.{List => _, Option => _, None => _, Either => _, _}

trait Monad[F[_]] extends Functor[F] {
  def point[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
  def map[A,B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => point(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
}


object Monad {
  trait Option[+A]
  case object None extends Option[Nothing]
  case class Some[+A](get: A) extends Option[A]

  val optionMonad = new Monad[Option] {
    def point[A](a: => A): Option[A] = None
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = map(ma)(f) match {
      case mb @ Some(b) => b//mb
      case _ => None
    }
  }
}
