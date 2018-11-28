package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class FoldLeftTest extends PropSpec with PropertyChecks with Matchers {

  property("Test foldLeft with sum function for Ints") {
    forAll { xs: Array[Int] =>
      val expected = xs.foldLeft(0)((z, x) => z + x)
      val actual = List.foldLeft(List(xs: _*), 0)((z, x) => z + x)
      actual should be(expected)
    }
  }

  property("Test foldLeft with cat function for Strings") {
    forAll { xs: Array[String] =>
      val expected = xs.foldLeft("")((z, x) => z + x)
      val actual = List.foldLeft(List(xs: _*), "")((z, x) => z + x)
      actual should be(expected)
    }
  }
}

