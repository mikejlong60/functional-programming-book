package chapter5

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class StreamTest extends PropSpec with PropertyChecks with Matchers {

  property("Test drop function for Stream of ints") {
    forAll { xs: Seq[Int] =>
      val a = Stream.apply(xs:_*)

      val actual = a.drop(1).toList
      val expected = xs.drop(1).toList
      actual should be (expected)
    }
  }

  property("Test take with 1 function for Stream of ints") {
    forAll { xs: Seq[Int] =>
      val a = Stream.apply(xs:_*)

      val actual = a.take(1).toList
      val expected = xs.take(1).toList
      actual should be (expected)
    }
  }

    property("Test take with 3 function for Stream of ints") {
    forAll { xs: Seq[Int] =>
      val a = Stream.apply(xs:_*)
      val actual = a.take(3).toList
      val expected = xs.take(3).toList
      actual should be (expected)
    }
  }

  property("Test toList function for Stream of Ints") {
    forAll { xs: Seq[Int] =>
      val a = Stream.apply(xs:_*)
      val actual = a.toList
       actual should be (xs)
    }
  }

}
