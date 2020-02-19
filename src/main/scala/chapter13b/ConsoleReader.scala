package chapter13b

case class ConsoleReader[A](run: String => A) {
  def map[B](f: A => B): ConsoleReader[B] = ConsoleReader(s => f(run(s)))
  def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] = ConsoleReader(s => f(run(s)).run(s))
}

object ConsoleReader {
  implicit val monad = new chapter11.Monad[ConsoleReader] {
    def unit[A](a: => A) = ConsoleReader(_ => a)
    def flatMap[A, B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]) = ra flatMap f
  }
}
