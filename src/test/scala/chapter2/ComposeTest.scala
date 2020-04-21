package chapter2

import org.scalacheck._

object ComposeSpecification extends Properties("Function Composition") {
  import Prop.forAll
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  def andThen[A, B, C](f: A => B, g: B => C): A => C = (a: A) => g(f(a))

  val f: Short => Long = (x: Short) => x.toLong
  val g: Long => Double = (x: Long) => x.toDouble
  property("Test function composition from right to left") =
    forAll { (n: Short) =>
      val gf = compose(g, f)
      gf(n) == (n.toDouble)
    }

  property("Test function composition from left to right") =
    forAll { (n: Short) =>
      val gf = andThen(f, g)

      gf(n) == (n.toDouble)
    }

  property("Test that compose and andThen are isomomorphic") =
    forAll { (n: Short) =>
      val fg = andThen(f, g)
      val gf = compose(g, f)

      fg(n) == (gf(n))
    }
}

