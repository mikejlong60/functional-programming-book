package chapter15

import java.io.{BufferedWriter, FileWriter}

import chapter13.IO
import chapter5.{Cons, Empty, Stream}


case class Emit[I, O](head: O, tail:  Process[I, O] = Halt[I, O]()) extends Process[I, O]

case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]

sealed trait Process[I, O] {
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream.empty
    case Await(recv) => s match {
      case Cons(h, t) => recv(Some(h()))(t())
      case Empty =>  recv(None)(Stream.empty)
    }
    case Emit(h, t) =>  Stream.cons(h, t(s))
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

  //I cheated on this
  def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
    case Halt() => Halt()
    case Emit(h1, t1) => Emit(h1, this |> t1)
    case Await(recv) => this match {
      case Emit(h,t) => t |> recv(Some(h))
      case Halt() => Halt() |> recv(None)
      case Await(g) => Await((i: Option[I]) => g(i) |> p2)
    }
  }

  def map[O2](f: O => O2): Process[I, O2] = this |> Process.lift(f)

  def ++(p: => Process[I, O]): Process[I, O] = this match {
    case Halt() => p
    case Emit(h, t) => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_  flatMap f))
  }
}

object Process {

  //I cheated on this too.
  def zip[I, O1, O2](p1:Process[I, O1])(p2: Process[I,O2]): Process[I,(O1,O2)] = (p1, p2) match {
    case (Halt() , _) => Halt()
    case (_, Halt()) => Halt()
    case (Emit(b, t1), Emit(c, t2)) => Emit((b,c), zip(t1)(t2))
    case (Await(recv), _) => Await((i: Option[I]) => zip(recv(i))(feed(i)(p2)))
    case (_, Await(recv)) => Await((i: Option[I]) => zip(feed(i)(p1))(recv(i)))

  }
  //Same. Too hard for now
  def feed[I, O](oa: Option[I])(p: Process[I, O]): Process[I, O] = p match {
    case Halt() => p
    case Emit(h, t) => Emit(h, feed(oa)(t))
    case Await(recv) => recv(oa)
  }

  val mean3 = Process.zip(Process.sum)(Process.count).map(t=> t._1 / t._2)

  val mean2 = Process.zip(Process.sumLoop)(Process.countLoop).map(t=> t._1 / t._2)

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

  def zipWithIndex[I]: Process[I, (I, Int)] = loop(-1)((i: I, s: Int) => ((i, 1 + s), (1 + s)))

  def count[I]: Process[I, Double] = {
    def go(acc: Double): Process[I, Double] =
      Await {
        case Some(d) => Emit (acc + 1, go(acc + 1))
        case None => Halt()
      }

    go(0)
  }

  def countLoop[I]: Process[I, Int] = loop(0)((_: I, s: Int) => (1 + s, 1 + s))
  
  def filter[I](p: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i)  if p(i) => Emit(i)
      case _ => Halt()
    }.repeat

  def exists[I](p: I => Boolean): Process[I, Boolean] =
    Await[I, Boolean] {
      case Some(i) => Emit(p(i))
      case _ => Halt()
    }.repeat

  def liftOne[I, O](f: I => O): Process[I, O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

}

object FileProcess {

  def processFile[A, B](f: java.io.File)(p: Process[String, A])(z: B)(g: (B, A) => B): IO[B] = IO {

    @annotation.tailrec
    def go(ss: Iterator[String], cur: Process[String, A], acc: B): B = {
      cur match {
        case Halt() => acc
        case Await(recv) =>
          val next = if (ss.hasNext) recv(Some(ss.next))
          else recv(None)
          go(ss, next, acc)
        case Emit(h, t) => go(ss, t, g(acc, h))
      }
    }
    val s = io.Source.fromFile(f)
    try go(s.getLines(), p, z)
    finally s.close
  }

}
