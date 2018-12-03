package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class HeadTest extends PropSpec with PropertyChecks with Matchers {

  property("Test head function for Ints") {
    forAll { xs: Array[Int] =>
      whenever(xs.size > 1) {
        val r = List.apply(xs: _*)
        List.head(r) should be (xs(0))
      }
    }
  }

  property("Test head function for Strings") {
    forAll { xs: Array[String] =>
      whenever(xs.size > 1) {
        val r = List.apply(xs: _*)
        List.head(r) should be (xs(0))
      }
    }
  }
}

