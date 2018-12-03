package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class AppendTest extends PropSpec with PropertyChecks with Matchers {

  property("Test append for Strings") {
    forAll { (xs1: Array[String], xs2: Array[String]) =>
      val expected = List(((xs1.toList ::: xs2.toList).toArray): _*)
      val actual = List.append(List(xs1: _*), List(xs2: _*))
      actual should be (expected)
    }
  }

  property("Test append for Ints") {
    forAll { (xs1: Array[Int], xs2: Array[Int]) =>
      val expected = List(((xs1.toList ::: xs2.toList).toArray): _*)
      val actual = List.append(List(xs1: _*), List(xs2: _*))
      actual should be (expected)
    }
  }
}
