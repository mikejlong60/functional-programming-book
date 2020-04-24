package chapter13b

sealed trait IOSimple[A] { self =>
  def run: A
  def map[B](f: A => B): IOSimple[B] = new IOSimple[B] {
    def run: B = f(self.run)
  }
  def flatMap[B](f: A => IOSimple[B]): IOSimple[B] = new IOSimple[B] {
    def run: B = f(self.run).run
  }
}

object IOSimple extends chapter11.Monad[IOSimple]  {
  def unit[A](a: => A): IOSimple[A] = new IOSimple[A] {
    def run = a
  }
  def flatMap[A, B](fa: IOSimple[A])(f: A => IOSimple[B]) = fa flatMap f
  def apply[A](a: => A): IOSimple[A] = unit(a)
}

object Converter {

  def ReadLine: IOSimple[String] = IOSimple { scala.io.StdIn.readLine() }
  def PrintLine(msg: String): IOSimple[Unit] = IOSimple {println(msg)}
  def fahrenheitToCelsius(f: Double) =  (f - 32) * 5.0/9.0

  def converter: IOSimple[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit.")
    d <- ReadLine.map(s => s.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def main(args: Array[String]): Unit = {
    converter.run
  }
}


