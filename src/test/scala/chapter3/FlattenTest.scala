package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class FlattenTest extends PropSpec with PropertyChecks with Matchers {

  property("Test flatten for Strings") {
    forAll { (xs1: Array[String], xs2: Array[String], xs3: Array[String]) =>
      val raw = List(List(xs1: _*), List(xs2: _*), List(xs3: _*))
      val expected = List((xs1.toList ::: xs2.toList ::: xs3.toList).toArray: _*)
      val actual = List.flatten(raw)
      actual should be (expected)
    }
  }

  property("Test flatten for Ints") {
    forAll { (xs1: Array[Int], xs2: Array[Int], xs3: Array[Int]) =>
      val raw = List(List(xs1: _*), List(xs2: _*), List(xs3: _*))
      val expected = List((xs1.toList ::: xs2.toList ::: xs3.toList).toArray: _*)
      val actual = List.flatten(raw)
      actual should be (expected)
    }
  }


}
