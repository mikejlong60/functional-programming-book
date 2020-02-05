package chapter13b

import language.higherKinds
import language.postfixOps

object TailRec {
  sealed trait TailRec[A] { self =>
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  def unit[A](a: => A): TailRec[A] = Return(a)
  def flatMap[A,B](a: TailRec[A])(f: A => TailRec[B]) = a flatMap f
  def suspend[A](a: => TailRec[A]) = Suspend(() => ()).flatMap {_ => a}

  @annotation.tailrec def run[A](io: TailRec[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap(a => g(a) flatMap f))
    }
  }
xs}
