package chapter13b

//import chapter7.nonblocking.Nonblocking.Par

object Free {

  //type TailRec[A] = Free[Function0, A]
  //type Async[A] = Free[Par, A]

  sealed trait Free[F[_], A] { self =>
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMapped(self, f)
    def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Pure(_)))
  }

  def freeMonad[F[_]]: chapter11.Monad[({type f[a] = Free[F, a]}) #f] = new chapter11.Monad[({type f[a] = Free[F, a]}) #f] {
    def flatMap[A,B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ma flatMap f
    def unit[A](a: => A): Free[F,A] = Pure(a)
  }

  case class Pure[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](a: F[A]) extends Free[F, A]
  case class FlatMapped[S[_], B, C](c: Free[S, C], f: C => Free[S, B]) extends Free[S, B]

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0,A]): A = (a) match {
    case Pure(a) => a
    case Suspend(r) => r()
    case FlatMapped(x,f) => x match {
      case Pure(a) => runTrampoline { f(a) }
      case Suspend(r) => runTrampoline { f(r()) }
      case FlatMapped(a0,g) => runTrampoline { a0 flatMap { a0 => g(a0) flatMap f } }
    }
  }

  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMapped(FlatMapped(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMapped(Pure(x), f) => step(f(x))
    case _ => a
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: chapter11.Monad[G]): G[A] = step(free) match {
    case Pure(a) => G.unit(a)
    case Suspend(r) => t(r) 
    case FlatMapped(x, f) => x match {
      case Suspend(r) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _  => sys.error("Impossible: `step` eliminates these case")
    }
  }


}
