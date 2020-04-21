package chapter3

import org.scalacheck._
import Prop.{forAll, propBoolean}

object SetHeadTest extends Properties("List set head") {

  property("Test setHead function for Ints") = {
    forAll { (xs: Array[Int], newHead: Int) =>
      (xs.size > 1) ==> {
        val r = List.apply(xs: _*)
        List.setHead(newHead, r) == (Cons(newHead, List.apply(xs.tail: _*)))
      }
    }
  }

  property("Test setHead function for Strings") = {
    forAll { (xs: Array[String], newHead: String) =>
      (xs.size > 1) ==> {
        val r = List.apply(xs: _*)
        List.setHead(newHead, r) == (Cons(newHead, List.apply(xs.tail: _*)))
      }
    }
  }
}

