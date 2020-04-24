package chapter13

import language.higherKinds
import language.implicitConversions

object TestIO {
  def ReadLine: IO[String] = IO { scala.io.StdIn.readLine()}
  def PrintLine(msg: String): IO[Unit] = IO {println(msg)}
  def fahrenheitToCelsius(f: Double) = (f - 32) * 5.0 / 9.0
  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees F")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

}

sealed trait IO[A] { self =>
  def run:A
  def map[B](f: A=> B): IO[B] =
    new IO[B] {
      def run = f(self.run)
    }
  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {
      def run = (f(self.run).run)
    }
}

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] {
    def run = a
  } 
  def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
  def apply[A](a: => A): IO[A] = unit(a)
}
