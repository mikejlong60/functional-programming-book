package chapter13b

//import chapter7.nonblocking.Nonblocking.Par

object Free {

  //type TailRec[A] = Free[Function0, A]
  //type Async[A] = Free[Par, A]

  sealed trait Free[F[_], A] { self =>
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(self, f)
    def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Pure(_)))
  }

  def freeMonad[F[_], A]: chapter11.Monad[({type f[a] = Free[F, a]}) #f] = new chapter11.Monad[({type f[a] = Free[F, a]}) #f] {
    def flatMap[A,B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ma flatMap f
    def unit[A](a: => A): Free[F,A] = Pure(a)
  }

  case class Pure[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  @annotation.tailrec
  def runTrampoline[F[_], A](a: Free[Function0, A]): A = a match {
    case Pure(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Pure(a) => runTrampoline (f(a))
      case FlatMap(s, g) => runTrampoline(s.flatMap(ss => g(ss).flatMap(f)))
      case Suspend(r) => runTrampoline(f(r()))
    }
  }

  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Pure(x), f) => step(f(x))
    case _ => a
  }

  def run[F[_], A](a: Free[F, A])(implicit F: chapter11.Monad[F]): F[A] = step(a) match {
    case Pure(a) => F.unit(a)
    case Suspend(r) => r 
    case FlatMap(x, f) => x match {
      case Suspend(r) => F.flatMap(r)(a => run(f(a)))
      case _  => sys.error("Impossible: `step` eliminates these case")
    }
  }

  import Console.~>

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: chapter11.Monad[G]): G[A] = step(free) match {
    case Pure(a) => G.unit(a)
    case Suspend(r) => t(r) 
    case FlatMap(x, f) => x match {
      case Suspend(r) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _  => sys.error("Impossible: `step` eliminates these case")
    }
  }


}
