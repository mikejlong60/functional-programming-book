package chapter13b

import Free._
import Console._
import java.util.concurrent._
import chapter7.nonblocking.Nonblocking.Par
//import java.nio._
//import java.nio.channels._

trait Source {
  def readBytes(numBytes: Int, callback: Either[Throwable, Array[Byte]] => Unit): Unit
}

object AsyncIO {
  def p: ConsoleState.ConsoleIO[Unit] = for {
    _ <- writeLn("What's your name")
    n <- readLn
    _ <- n match {
      case Some(n) => writeLn(s"Hello, $n!")
      case None => writeLn(s"Fine, be that way.")
    }
  } yield ()

  def nonblockingRead(source: Source, numBytes: Int): Par[Either[Throwable, Array[Byte]]] = Par.async{
    (cb: Either[Throwable, Array[Byte]] => Unit) => source.readBytes(numBytes, cb)
  }

  def readPar(source: Source, numBytes: Int): Free[Par, Either[Throwable, Array[Byte]]] = Free.Suspend(nonblockingRead(source, numBytes))


  def main(args: Array[String]): Unit = {
    val executor = Executors.newFixedThreadPool(60)

    val par = Console.runConsolePar(p)
    Par.run(executor)(par).get


    val s = new Source {
      def readBytes(numBytes: Int, callback: Either[Throwable, Array[Byte]] => Unit): Unit = println("this really makes me mad!!!")
    }
    val r2 = readPar(s, 12)

    val prog: Free.Free[Par, Unit] = for {
      chunk1 <- readPar(s, 10)
      chunk2 <- readPar(s, 10)
    }  yield (chunk1, chunk2)

   // val parToConsole = new (Par ~> Console) {
   //   def apply[A](a: Par[A]) =a match {
   //     case Par(a) => _
   //   }//Par.flatMap(a)(p => ConsoleReader(a => Par.lazyUnit(p)))///Some(p)))// a.toConsole//Par.unit(a)//fork{Par.flatMap(a)(println("hi")}
  //}


    //Free.runFree(prog)(Console.consoleToReader)
  }
}
