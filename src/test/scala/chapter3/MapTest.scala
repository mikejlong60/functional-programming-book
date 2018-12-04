package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class MapTest extends PropSpec with PropertyChecks with Matchers {

  property("Test add function that preserves list") {
    forAll { xs: Array[Int] =>
      val expected = xs.map(x => x + 1)
      val actual = List.foldRight(List(xs: _*), Nil: List[Int])((x, z) => Cons(x + 1, z))
      actual should be(List(expected: _*))
    }
  }

  property("Test convert function that preserves list") {
    forAll { xs: Array[Double] =>
      val expected = xs.map(x => x.toString)
      val actual = List.foldRight(List(xs: _*), Nil: List[String])((x, z) => Cons(s"$x", z))
      actual should be(List(expected: _*))
    }
  }

  property("Test local map-like function that preserves list") {
    val str = (x: Double) => s"$x"
    forAll { xs: Array[Double] =>
      val expected = xs.map(x => x.toString)
      val actual = List.foldRight(List(xs: _*), Nil: List[String])((x, z) => Cons(str(x), z))
      actual should be(List(expected: _*))
    }
  }

  property("Test real map function that preserves list") {
    val str = (x: Double) => s"$x"
    forAll { xs: Array[Double] =>
      val expected = xs.map(x => x.toString)
      val actual = List.map(List(xs: _*))(str)
      actual should be(List(expected: _*))
    }
  }

}
