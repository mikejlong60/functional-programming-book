package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class SumProductLengthTest extends PropSpec with PropertyChecks with Matchers {

  property("Test sum function that uses foldLeft") {
    forAll { xs: Array[Int] =>
      val expected = xs.foldLeft(0)((z, x) => z + x)
      val actual = List.foldLeft(List(xs: _*), 0)((z, x) => z + x)
      actual should be(expected)
    }
  }

  property("Test product function that uses foldLeft") {
    forAll { xs: Array[Int] =>
      val expected = xs.foldLeft(0)((z, x) => z * x)
      val actual = List.foldLeft(List(xs: _*), 0)((z, x) => z * x)
      actual should be(expected)
    }
  }

  property("Test length function that uses foldLeft") {
    forAll { xs: Array[Int] =>
      val expected = xs.foldLeft(0)((z, x) => z + 1)
      val actual = List.foldLeft(List(xs: _*), 0)((z, x) => z + 1)
      actual should be(expected)
    }
  }
}

