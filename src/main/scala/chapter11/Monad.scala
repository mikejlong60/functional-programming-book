package chapter11

import scala.{List => _, Option => _, None => _, Either => _, _}

trait Monad[F[_]] extends Functor[F] {
  def point[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
  def map[A,B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => point(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
}


object Option {
  trait Option[+A]
  case object None extends Option[Nothing] {
    println("this is my none")
  }
  case class Some[+A](get: A) extends Option[A] {
    println("this is my some")
  }

  val option = new Monad[Option] {
    def point[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma match {
      case Some(a) => f(a)
      case None => None
    }
  }
}
