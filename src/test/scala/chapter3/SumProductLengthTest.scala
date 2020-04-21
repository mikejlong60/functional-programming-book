package chapter3

import org.scalacheck._
import Prop.{forAll, propBoolean}

object SumProductLengthTest extends Properties("List sum product") {

  property("Test sum function that uses foldLeft") =
    forAll { xs: Array[Int] =>
      val expected = xs.foldLeft(0)((z, x) => z + x)
      val actual = List.foldLeft(List(xs: _*), 0)((z, x) => z + x)
      actual == (expected)
    }

  property("Test product function that uses foldLeft") =
    forAll { xs: Array[Int] =>
      val expected = xs.foldLeft(0)((z, x) => z * x)
      val actual = List.foldLeft(List(xs: _*), 0)((z, x) => z * x)
      actual ==(expected)
    }

  property("Test length function that uses foldLeft") =
    forAll { xs: Array[Int] =>
      val expected = xs.foldLeft(0)((z, x) => z + 1)
      val actual = List.foldLeft(List(xs: _*), 0)((z, x) => z + 1)
      actual == (expected)
    }
}

