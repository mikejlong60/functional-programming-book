package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class TailTest extends PropSpec with PropertyChecks with Matchers {

  property("Test tail function for Ints") {
    forAll { xs: Array[Int] =>
      whenever(xs.size > 1) {
        val r = List.apply(xs: _*)
        List.tail(r) should be(List.apply(xs.tail: _*))
      }
    }
  }

  property("Test tail function for Strings") {
    forAll { xs: Array[String] =>
      whenever(xs.size > 1) {
        val r = List.apply(xs: _*)
        List.tail(r) should be(List.apply(xs.tail: _*))
      }
    }
  }
}

