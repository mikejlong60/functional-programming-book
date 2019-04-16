package chapter13

object IOOps {

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0/9.0

  def ReadLine: IO[String] = IO { readLine }

  def PrintLine(msg: String): IO[Unit] = IO {println(msg)}

  def converter: IO[Unit] = for {
    _  <- PrintLine("enter a temperature in degrees fahrenheit:")
    d <- ReadLine.map(l => l.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

}
