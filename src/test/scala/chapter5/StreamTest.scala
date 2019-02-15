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

  val p: Int => Boolean = x => x % 2 == 1

  property("Test takewhile for Stream of ints") {
    forAll { xs: Seq[Int] =>
     val actual = Stream.apply(xs:_*).takeWhile(p)
     actual.toList should be (xs.takeWhile(p))
    }
  }

  property("Test toList function for Stream of Ints") {
    forAll { xs: Seq[Int] =>
      val a = Stream.apply(xs:_*)
      val actual = a.toList
       actual should be (xs)
    }
  }

  property("Test exists function for Stream of Ints") {
    forAll { xs: Seq[Int] =>
      val actual  = Stream.apply(xs:_*)
       actual.exists(p) should be (xs.exists(p))
    }
  }

    property("Test forAll function for Stream of Ints") {
    forAll (minSuccessful(8000), maxDiscarded(300)){ xs: Seq[Int] =>
      val actual  = Stream.apply(xs:_*)
       actual.forAll(p) should be (xs.forall(p))
    }
  }

  val f: Int => Int = _ *10
  property("Test map function for Stream of Ints") {
    forAll { xs: Seq[Int] =>
      val actual  = Stream.apply(xs:_*)
       (actual.map(f).toList) should be (xs.map(f))
    }
  }

    property("Test append function for Stream of Ints") {
    forAll {(xs: Seq[Int], ys: Seq[Int]) =>
      val a1  = Stream.apply(xs:_*)
      val a2  = Stream.apply(ys:_*)
      val expected = xs ++ ys
       (a1.append(a2).toList) should be (expected)
    }
  }

}
