package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class SetHeadTest extends PropSpec with PropertyChecks with Matchers {

  property("Test setHead function for Ints") {
    forAll { (xs: Array[Int], newHead: Int) =>
      whenever(xs.size > 1) {
        val r = List.apply(xs: _*)
        List.setHead(newHead, r) should be(Cons(newHead, List.apply(xs.tail: _*)))
      }
    }
  }

  property("Test setHead function for Strings") {
    forAll { (xs: Array[String], newHead: String) =>
      whenever(xs.size > 1) {
        val r = List.apply(xs: _*)
        List.setHead(newHead, r) should be(Cons(newHead, List.apply(xs.tail: _*)))
      }
    }
  }
}

