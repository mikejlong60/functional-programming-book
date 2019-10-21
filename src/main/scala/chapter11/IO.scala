package chapter11

trait IO[A] { self =>
  def run: A
}
