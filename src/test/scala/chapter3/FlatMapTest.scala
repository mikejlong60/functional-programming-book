package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class FlatMapTest extends PropSpec with PropertyChecks with Matchers {

  property("Test flatMap that doubles each element and flattens it into a single list") {
    forAll { xs: Array[Int] =>
      val expected = xs.flatMap(x => Array(x ,x))
      val actual = List.flatMap(List(xs: _*))(x => List(x, x))
      actual should be(List(expected: _*))
    }
  }

  property("Test filterUsingFlatmap function to remove odd elements from list of ints") {
    val filtF = (x: Int) => x % 2 == 0
    forAll { xs: Array[Int] =>
      val expected = xs.filter(filtF)
      val actual = List.filterUsingFlatMap(List(xs: _*))(filtF)
      actual should be(List(expected: _*))
    }
  }

  property("Test filterUsingFlatMap function to remove elements with size > 10 from list of Strings") {
    val filtF = (x: String) => x.length < 11
    forAll { xs: Array[String] =>
      val expected = xs.filter(filtF)
      val actual = List.filterUsingFlatMap(List(xs: _*))(filtF)
      actual should be(List(expected: _*))
    }
  }

  property("chain functions using flatMap") {
    forAll { xs: Array[Int] =>
      val expected = xs.flatMap(x => Array(x ,x))
      val actual = List.flatMap(List.flatMap(List(xs: _*))(x => List(x, x)))(x => (List(x,x,x)))
      println(actual)
      //actual should be (empty)//be(List(expected: _*))
    }

  }
}
