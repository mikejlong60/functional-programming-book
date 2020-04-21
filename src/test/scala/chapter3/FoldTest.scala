package chapter3

import org.scalacheck._
import Prop.{forAll, propBoolean}

object FoldTest extends Properties("List Folding") {

  property("Test foldLeft with sum function for Ints")  =
    forAll { xs: Array[Int] =>
      val expected = xs.foldLeft(0)((z, x) => z + x)
      val actual = List.foldLeft(List(xs: _*), 0)((z, x) => z + x)
      actual == (expected)
    }

  property("Test foldLeft with cat function for Strings") =
    forAll { xs: Array[String] =>
      val expected = xs.foldLeft("")((z, x) => z + x)
      val actual = List.foldLeft(List(xs: _*), "")((z, x) => z + x)
      actual == (expected)
    }

  property("Test foldRight with cat function for Strings") =
    forAll { xs: Array[String] =>
      val expected = xs.foldRight("")((z, x) => z + x)
      val actual = List.foldRight(List(xs: _*), "")((z, x) => z + x)
      actual == (expected)
    }

  property("Test foldRightTailRec with cat function for Strings") =
    forAll { xs: Array[String] =>
      val expected = xs.foldRight("")((z, x) => z + x)
      val actual = List.foldRightTailRec(List(xs: _*), "")((z, x) => z + x)
      actual == (expected)
    }

  property("Test that the Cons constructor is isomorphic to the data constructors of List with foldRight.  This is not true for foldLeft because the order of the arguments is switched with the accumulator on the left.") =
    forAll { xs: Array[Int] =>
      val expected = List.foldRight(List(xs: _*), Nil: List[Int])((x, z) => Cons(x, z))
      val actual = List.foldRight(List(xs: _*), Nil: List[Int])(Cons(_, _))
      actual == (expected)
    }

  property("Test that the Cons constructor is isomorphic to the data constructors of List with foldRightTailRec.  This is not true for foldLeft because the order of the arguments is switched with the accumulator on the left.") =
    forAll { xs: Array[Int] =>
      val expected = List.foldRightTailRec(List(xs: _*), Nil: List[Int])((x, z) => Cons(x, z))
      val actual = List.foldRightTailRec(List(xs: _*), Nil: List[Int])(Cons(_, _))
      actual == (expected)
    }

  property("Test that foldLeft reverses the order of the list with Cons") =
    forAll { xs: Array[Int] =>
      val xsReversed = xs.reverse
      val expected = List(xsReversed: _*)
      val actual = List.reverse(List(xs: _*))
      actual ==(expected)
    }

  property("Test length using foldRight") = {
    val length = (x: Int, z: Int) => z + 1
    forAll { xs: Array[Int] =>
      val expected = xs.length
      val actual = List.foldRight(List(xs: _*), 0)(length)
      actual == (expected)
    }
  }

  property("Test length using foldRightTailRec") = {
    val length = (x: Int, z: Int) => z + 1
    forAll { xs: Array[Int] =>
      val expected = xs.length
      val actual = List.foldRightTailRec(List(xs: _*), 0)(length)
      actual == (expected)
    }
  }
}
