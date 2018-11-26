package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DropTest extends PropSpec with PropertyChecks with Matchers {

  property("Test drop function for Ints") {
    forAll { (xs: Array[Int], n: Short) =>
      whenever(n < 100 && n >= 0) {
        val l = List.apply(xs: _*)
        val expected = List.apply(xs.drop(n): _*)
        val actual = List.drop(l, n)
        actual should be (expected)
      }
    }
  }

  property("Test drop function for Strings") {
    forAll { (xs: Array[String], n: Short) =>
      whenever(n < 100 && n >= 0) {
        val l = List.apply(xs: _*)
        val expected = List.apply(xs.drop(n): _*)
        val actual = List.drop(l, n)
        actual should be (expected)
      }
    }
  }


}

