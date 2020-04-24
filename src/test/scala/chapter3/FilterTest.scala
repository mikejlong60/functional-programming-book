package chapter3

import org.scalacheck._
import Prop.{forAll, propBoolean}

object FilterTest extends Properties("List Filter") {

  property("Test filter function to remove odd elements from list of ints") = {
    val filtF = (x: Int) => x % 2 == 0
    forAll { xs: Array[Int] =>
      val expected = xs.filter(filtF)
      val actual = List.filter(List(xs: _*))(filtF)
      actual == (List(expected: _*))
    }
  }

  property("Test filter function to remove elements with size > 10 from list of Strings") = {
    val filtF = (x: String) => x.length < 11
    forAll { xs: Array[String] =>
      val expected = xs.filter(filtF)
      val actual = List.filter(List(xs: _*))(filtF)
      actual == (List(expected: _*))
    }
  }
}