package chapter13b

import chapter7.nonblocking.Nonblocking.Par

object Free {

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  sealed trait Free[F[_], A] { self =>
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(self, f)
    def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
  }

  def freeMonad[F[_], A]: chapter11.Monad[({type f[a] = Free[F, a]}) #f] = new chapter11.Monad[({type f[a] = Free[F, a]}) #f] {
    def flatMap[A,B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ma flatMap f
    def unit[A](a: => A): Free[F,A] = Return(a)
  }

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  def runTrampoline[F[_], A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline (f(a))
      case FlatMap(s, g) => runTrampoline(s.flatMap(ss => g(ss).flatMap(f)))
      case Suspend(r) => runTrampoline(f(r()))
    }
  }
}
