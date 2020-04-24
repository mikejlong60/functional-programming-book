package chapter13b

import java.util.concurrent._
import chapter7.nonblocking.Nonblocking.Par
import org.scalacheck._
import Prop.{forAll, propBoolean}

object ConsoleTest extends Properties("Console test") {

  def runConsoleFunction0[A](a: Free.Free[Console, A]): () => A = Free.runFree[Console, Function0, A](a)(Console.consoleToFunction0)(Console.function0Monad)

  import Free._

  type ConsoleIO[A] = Free.Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Free.Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] = Free.Suspend(PrintLine(line))

  property("Goof around with Free using Console ") =
    forAll { x: Short =>
      val f1: Free.Free[Console, Option[String]] = for {
        _ <- printLn("I can only interact with the console.")
        ln <- readLn
      } yield ln


      //TODO This runs but blocks the other tests because it requires you to hit "return".  Will replace it later with a non-blocking version.
      val actual = Free.runFree(f1)(Console.consoleToFunction0)(Console.function0Monad)
      //val result = actual()
      //println(result)

      true
    }

  property("Goof around with Exercise 13.4 ") =
    forAll{ xs: List[String] =>
      val result = xs.map(s => Console.runConsole(printLn(s)))
      result.size == xs.size
    }
}
