package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DropWhileTest extends PropSpec with PropertyChecks with Matchers {

  property("Test dropWhile function for Ints") {
    forAll { xs: Array[Int] =>
      val expected = xs.dropWhile(x => x > 100)
      List.dropWhile(List(xs: _*))(x => x > 100) should be(List(expected: _*))
    }
  }
  property("Test dropWhile function for Strings") {
    forAll { xs: Array[String] =>
      val expected = xs.dropWhile(x => x.length > 10)
      List.dropWhile(List(xs: _*))(x => x.length > 10) should be(List(expected: _*))
    }
  }
}

