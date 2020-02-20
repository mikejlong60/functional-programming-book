package chapter13b

import chapter7.nonblocking.Nonblocking.Par
import Free.~>

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
  def toReader: ConsoleReader[A]
  def toState: ConsoleState[A]
}

case object ReadLine extends Console[Option[String]] {
  def toPar = Par.lazyUnit(run)
  def toThunk = () => run
  def toReader: ConsoleReader[Option[String]] = ConsoleReader( s => Some(s))
  def toState: ConsoleState[Option[String]] = ???

  def run: Option[String] =
    try Some(readLine())
    catch { case e: Exception => None}
}

case class PrintLine(line: String) extends Console[Unit] {
  def toPar = Par.lazyUnit(println(line))
  def toThunk = () => println(line)
  def toReader: ConsoleReader[Unit] = ConsoleReader(s => ())
  def toState: ConsoleState[Unit] = ???
}

trait Translate[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}


object Console {
  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] = Free.runFree[Console, ConsoleReader, A](io)(consoleToReader)

  val consoleToReader = new (Console ~> ConsoleReader) {
    def apply[A](a: Console[A]) = a.toReader
  }

  val function0Monad = new chapter11.Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A, B](a: Function0[A])(f: A => Function0[B]) = () => f(a())()
  }

  //type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFunction0 = new (Console ~> Function0) {
    def apply[A](a: Console[A]) = a.toThunk
  }

  val consoleToPar = new (Console ~> Par) {
    def apply[A](a: Console[A]) = a.toPar
  }

  //Exercise 13.4
  def runConsole[A](a: Free.Free[Console,A]): A = Free.runTrampoline(translate(a)(consoleToFunction0))
  def translate[F[_], G[_], A](f: Free.Free[F, A])(fg: F ~> G): Free.Free[G, A] = {
    type FreeG[A] = Free.Free[G,A]
    val t = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free.Free[G, A] = Free.Suspend {fg(a)}
    }
    Free.runFree(f)(t)(Free.freeMonad[G])
  }

  type ConsoleIO[A] = Free.Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Free.Suspend(ReadLine)

  def writeLn(line: String): ConsoleIO[Unit] = Free.Suspend(PrintLine(line))

  def main(args: Array[String]) = {

   val f1: Free.Free[Console, Option[String]] = for {
      _  <- writeLn("I can only interact with the console.")
      ln <- readLn
    } yield ln

      val actual: () => Option[String] = Free.runFree(f1)(consoleToFunction0)(function0Monad)
      val result = actual()
      println(result)
  }
}
