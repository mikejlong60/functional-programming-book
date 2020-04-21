package chapter15.generic

import java.io.{BufferedReader, FileReader}
import java.util.concurrent.ExecutorService

import Process._
import chapter13.IO
import chapter13b.ReadLine.run
import chapter7.nonblocking.Nonblocking.Par

trait Process[F[_],O] {
  def onHalt(f: Throwable => Process[F,O]): Process[F,O] = this match {
    case Halt(e) => Try(f(e))
    case Emit(h, t) => Emit(h, t.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
  }

  def ++(p: => Process[F,O]): Process[F,O] =
    this.onHalt {
      case End => p
      case err => Halt(err)
    }

  def flatMap[O2](f: O => Process[F,O2]): Process[F,O2] = this match {
    case Halt(err) => Halt(err)
    case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
    case Await(req, recv) => Await(req, recv andThen (_ flatMap f))
  }
}

object Process {

  case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]

  case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]

  case class Halt[F[_], O](err: Throwable) extends Process[F, O]

  case object End extends Exception

  case object Kill extends Exception

  def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
    try p
    catch {
      case e: Throwable => Halt(e)
    }

  def await[F[_], A, O](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] = Await(req, recv)

  def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] = IO {
    val E = java.util.concurrent.Executors.newFixedThreadPool(4)

    //def unsafePerformIO[F[_], A](f: F[A])(E: ExecutorService): A = {
    //  val t = Par.run(E) {
    ///    Try(f)
     // }

   // f()//t
      //()
    //}
  def unsafePerformIO[A](f: IO[A])(e: ExecutorService) = {
        val fl = Par.lazyUnit(f)
        Par.run(e)(fl)
        //}//.get//e.??? //TODO look at your par package
      }

    @annotation.tailrec
    def go(cur: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] = cur match {
      case Emit(h, t) => go(t, acc :+ h) //remember mnemonic, the colon goes on the collection side.
      case Halt(End) => acc
      case Halt(err) => throw err
      case Await(req, recv) =>
        val next =
          try recv(Right(req.run))//unsafePerformIO(req)(E)))
//        try recv(Right(unsafePerformIO(req)(E)))
          catch {
            case err: Throwable => recv(Left(err))
          }
        go(next, acc)
    }

    try go(src, IndexedSeq())
    finally E.shutdown()
  }

  val p: Process[IO, String] =
    await(IO(new BufferedReader(new FileReader("temp.txt")))) {
      case Right(b) =>
        lazy val next: Process[IO, String] = await(IO(b.readLine())) {
          case Left(e) => await(IO(b.close))(_ => Halt(e))
          case Right(line) =>
            if (line eq null) Halt(End)
            else Emit(line, next)
        }
        next
      case Left(e) => Halt(e)
    }
}

