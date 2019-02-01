package chapter5

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class StreamTest extends PropSpec with PropertyChecks with Matchers {

  property("Test drop function for Stream of ints") {
    forAll { xs: Seq[Int] =>
      val a = Stream.apply(xs:_*)

      val actual = Stream.drop(1)(a)
      val expected = xs match {
       case x :: xs => Stream.apply(xs:_*)
       case _ => Empty
      }
      actual should be (expected)
    }
  }

  property("Test toList function for Stream of Ints") {
    forAll { xs: Seq[Int] =>
      val a = Stream.apply(xs:_*)
      val actual = Stream.toList(a)
       actual should be (xs)
    }

  }
}
