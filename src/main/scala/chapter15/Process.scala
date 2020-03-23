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

  def takeWhile[I](f: I => Boolean): Process[I, I] = {
      def go: Process[I, I] = 
        Await {
          case Some(d) if f(d) => Emit(d, go)
          case _ => Halt()
        }

    go
  }

  def dropWhile[I](f: I => Boolean): Process[I, I] = {
      def go(firstFailure: Boolean): Process[I, I] = 
        Await {
          case Some(d) if f(d) => if (firstFailure) Emit(d, go(true)) else go(false)
          case Some(d)  => Emit(d, go(true))
          case _ => Halt()
        }
 
    go(false)
  }

  def drop[I](n: Int): Process[I, I] = {
      def go(cnt: Int): Process[I, I] = 
        Await {
          case Some(d) if cnt < n => go(cnt + 1)
          case Some(d)  => Emit(d, go(cnt  + 1))
          case _ => Halt()
        }

    go(0)
  }

  def take[I](n: Int): Process[I, I] = {
      def go(cnt: Int): Process[I, I] = 
        Await {
          case Some(d) if cnt < n => Emit(d, go(cnt + 1))
          case _ => Halt()
        }

    go(0)
  }

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit (d + acc, go(d + acc))
        case None => Halt()
      }

    go(0.0)
  }

  def mean: Process[Double, Double] = {
    def go(currentTotal: Double, count: Int): Process[Double, Double] =
      Await {
        case Some(d) => Emit ((currentTotal+ d)/ count, go(currentTotal + d, count + 1))
        case None => Halt()
      }

    go(0, 1)
  }

  def emit[I, O](head: O, tail: Process[I, O] = Halt[I,O]()): Process[I, O] = Emit(head, tail)

   def await[I,O](f: I => Process[I,O], fallback: Process[I,O] = Halt[I,O]()): Process[I,O] =
     Await[I,O] {
       case Some(i) => f(i)
       case None => fallback
     }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    await((i: I) => f(i, z) match {
      case (o, z2) => emit(o, loop(z2)(f))
    })

  def sumLoop: Process[Double, Double] = loop(0.0)((i: Double, s: Double) => (i + s, i + s))
  
  def count[I]: Process[I, Int] = {
    def go(acc: Int): Process[I, Int] =
      Await {
        case Some(d) => Emit (acc + 1, go(acc + 1))
        case None => Halt()
      }

    go(0)
  }

  def countLoop[I]: Process[I, Int] = loop(0)((_: I, s: Int) => (s + 1, s + 1))
  
  def filter[I](p: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i)  if p(i) => Emit(i)
      case _ => Halt()
    }.repeat

  def liftOne[I, O](f: I => O): Process[I, O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

}

case class Emit[I, O](head: O, tail:  Process[I, O] = Halt[I, O]()) extends Process[I, O]

case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]
