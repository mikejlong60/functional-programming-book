package chapter3

import org.scalacheck._
import Prop.{forAll, propBoolean}

object DropTest extends Properties("List Drop") {

  property("Test drop function for Ints") =
    forAll { (xs: Array[Int], n: Short) =>
      (n < 100 && n >= 0) ==> {
        val l = List.apply(xs: _*)
        val expected = List.apply(xs.drop(n): _*)
        val actual = List.drop(l, n)
        actual == expected
      }
    }

  property("Test drop function for Strings") =
    forAll { (xs: Array[String], n: Short) =>
      (n < 100 && n >= 0) ==> {
        val l = List.apply(xs: _*)
        val expected = List.apply(xs.drop(n): _*)
        val actual = List.drop(l, n)
        actual == expected
      }
    }
}

