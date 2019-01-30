package chapter4

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scala.{List => _, Option => _, None => _, Some => _,  Either => _, _}
import chapter3.List
import chapter3.Cons
import chapter3.Nil

class EitherTest extends PropSpec with PropertyChecks with Matchers {

  property("Test map function for Ints") {
    forAll { x: Int =>
      val actual = Right(x).map(x => x + 1)
      val expected = Right(x + 1)
      actual should be (expected)
    }
  }

  property("Test flatMap function for Ints") {
    forAll { x: Int =>
      val actual = Right(x).flatMap(x => Right(x + 1))
      val expected = Right(x + 1)
      actual should be (expected)
    }
  }

  property("Test orElse function for Ints") {
    val actual = Left(new Exception("woops")).orElse(Left("woops"))
    val expected = Left("woops")
    actual should be (expected)
  }

   property("Test map2 function for Ints") {
    forAll {(x: Int, y: Int) =>
      val a = Right(x)
      val b = Right(y)

      val actual = a.map2(b)((a, b) => a + b)
      val expected = Right(x + y)
      actual should be (expected)
    }
  }

  property("Test map2 function for Ints where one is a Left with an error message, not an exception") {
    forAll {x: Int =>
      val a = Right(x)
      val b = Left("you are busted")

      val actual = a.map2(b)((a, b) => a + 12)
      val expected = Left("you are busted")
      actual should be (expected)
    }
  }

   property("Test map2 function for Ints using Try when one throws an exception") {
    forAll {(x: Int, y: String) =>
      val a = Right(x)
      val f: String => Int = (s: String) => s.toInt
      val b = Either.Try(f(y))
      val actual = a.map2(b)((a, b) => a + b)
        actual shouldBe an [Left[_]]
    }
  }


}

