package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ZipWithTest extends PropSpec with PropertyChecks with Matchers {

  property("Test zipWith with sum function for Ints") {
    forAll { (xs1: Array[Int], xs2: Array[Int]) =>
      val expected = (xs1, xs2).zipped.toList.map(xy => xy._1 + xy._2)
      val actual = List.zipWith(List(xs1: _*), List(xs2: _*))((z, x) => z + x)
      actual should be (List(expected: _*))
    }
  }

  property("Test zipWith with sum function for Ints and Strings") {
    forAll { (xs1: Array[Int], xs2: Array[String]) =>
      val expected = (xs1, xs2).zipped.toList.map(xy => s"${xy._1} + ${xy._2}")
      val actual = List.zipWith(List(xs1: _*), List(xs2: _*))((z, x) => s"$z + $x")
      actual should be (List(expected: _*))
    }
  }
}
