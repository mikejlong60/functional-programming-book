package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class FilterTest extends PropSpec with PropertyChecks with Matchers {

  property("Test filter function to remove odd elements from list of ints") {
    val filtF = (x: Int) => x % 2 == 0
    forAll { xs: Array[Int] =>
      val expected = xs.filter(filtF)
      val actual = List.filter(List(xs: _*))(filtF)
      actual should be(List(expected: _*))
    }
  }

  property("Test filter function to remove elements with size > 10 from list of Strings") {
    val filtF = (x: String) => x.length < 11
    forAll { xs: Array[String] =>
      val expected = xs.filter(filtF)
      val actual = List.filter(List(xs: _*))(filtF)
      actual should be(List(expected: _*))
    }
  }
}