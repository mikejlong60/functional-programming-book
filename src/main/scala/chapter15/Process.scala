package chapter15

import chapter5.{Stream, Cons, Empty}

sealed trait Process[I, O] {
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream.empty
    case Await(recv) => s match {
      case Cons(h, t) => recv(Some(h()))(t())
      case Empty => recv(None)(Stream.empty)
    }
    case Emit(h, t) => Stream.cons(h, t(s))
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }

    go(this)
  }
}

object Process {
  def liftOne[I, O](f: I => O): Process[I, O] = Await  { 
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat
}

case class Emit[I, O](head: O, tail:  Process[I, O] = Halt[I, O]()) extends Process[I, O]

case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]
