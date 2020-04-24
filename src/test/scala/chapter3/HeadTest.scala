package chapter3

import org.scalacheck._
import Prop.{forAll, propBoolean}

object HeadTest extends Properties("List head") {

  property("Test head function for Ints") =
    forAll { xs: Array[Int] =>
      (xs.size > 1) ==> {
        val r = List.apply(xs: _*)
        List.head(r) == (xs(0))
      }
    }

  property("Test head function for Strings") =
    forAll { xs: Array[String] =>
      (xs.size > 1) ==> {
        val r = List.apply(xs: _*)
        List.head(r) == (xs(0))
      }
    }
}

