package chapter3

import org.scalacheck._
import Prop.{forAll, propBoolean}

object TailTest extends Properties("List tail stuff") {

  property("Test tail function for Ints") = {
    forAll { xs: Array[Int] =>
      (xs.size > 1) ==> {
        val r = List.apply(xs: _*)
        List.tail(r) == (List.apply(xs.tail: _*))
      }
    }
  }

  property("Test tail function for Strings") = {
    forAll { xs: Array[String] =>
      (xs.size > 1) ==> {
        val r = List.apply(xs: _*)
        List.tail(r) == (List.apply(xs.tail: _*))
      }
    }
  }
}

