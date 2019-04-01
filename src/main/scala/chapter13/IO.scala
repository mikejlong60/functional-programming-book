package chapter13

trait IO[A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] = new IO[B] {
     def run = f(self.run)
  }
  def flatMap[B](f : A => IO[B]): IO[B] = new IO[B]  {
    def run = f(self.run).run 
  }
  def ++(io: IO[A]): IO[A] = new IO[A] {
    def run = {
      self.run
      io.run
    }
  }
}

object IO extends Monad[IO] {
   def unit[A](a: => A): IO[A] = new IO[A] {
     def run = a
   }
   def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
   def apply[A](a: => A): IO[A] =unit(a)
}

object Extras {
   def PrintLine(msg: String): IO[Unit] =  IO (println(msg))//this is the apply method above    
   def ReadLine: IO[String] = IO {readLine}
   def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0
   def converter: IO[Unit] = for {
       _ <- PrintLine("Enter a temperature in degrees fahrenheit:")
       d <- ReadLine.map(x => x.toDouble)
       _ <- PrintLine(fahrenheitToCelsius(d).toString())
   } yield ()

  def factorial(n: Int): IO[Int] = for {
    acc <- ref(1)
    _ <- foreachM (1 to n toStream)
  }
}
