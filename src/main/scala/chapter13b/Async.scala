package chapter13b

//import language.higherKinds
//import language.postfixOps
import chapter7.nonblocking.Nonblocking.Par

object Async {
  sealed trait Async[A] { self =>
    def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(self, f)
    def map[B](f: A => B): Async[B] = flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A]
  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  @annotation.tailrec def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a) => Par.unit(a)
    case Suspend(r) => r 
    case FlatMap(x, f) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
      case _  => sys.error("Impossible: `step` eliminates these case")
    }
  }
}
