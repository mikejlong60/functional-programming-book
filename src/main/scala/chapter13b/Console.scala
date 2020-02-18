package chapter13b

import chapter7.nonblocking.Nonblocking.Par

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {
  def toPar = Par.lazyUnit(run)
  def toThunk = () => run

  def run: Option[String] =
    try Some(readLine())
    catch { case e: Exception => None}
}

case class PrintLine(line: String) extends Console[Unit] {
  def toPar = Par.lazyUnit(println(line))
  def toThunk = () => println(line)
}

trait Translate[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}


object Console {
  type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFunction0 = new (Console ~> Function0) {
    def apply[A](a: Console[A]) = a.toThunk
  }

  val consoleToPar = new (Console ~> Par) {
    def apply[A](a: Console[A]) = a.toPar
  }

  def step[F[_], G[_], A](free: Free.Free[F, A]): Free.Free[F,A] = ???

  def runFree[F[_], G[_], A](free: Free.Free[F, A])(t: F ~> G)(implicit G: chapter11.Monad[G]): G[A] = step(free) match {
    case Free.Pure(a) => G.unit(a)
    case Free.Suspend(r) => t(r)
    case Free.FlatMapped(Free.Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
    case _ => sys.error("Impossible; `step` eliminates these cases")
  }

  type ConsoleIO[A] = Free.Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Free.Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] = Free.Suspend(PrintLine(line))

  def main(args: Array[String]) = {

   val f1: Free.Free[Console, Option[String]] = for {
      _  <- printLn("I can only interact with the console.")
      ln <- readLn
    } yield ln
  }
}
