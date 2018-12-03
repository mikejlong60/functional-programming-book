package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class FlattenTest extends PropSpec with PropertyChecks with Matchers {

  property("Test flatten for Strings") {
    forAll { (xs: Array[Array[String]]) =>
      val expected = List(xs.flatten: _*)
//      val actual = List.append(List(xs1: _*), List(xs2: _*))
//      actual should be (expected)
    }
  }
}
