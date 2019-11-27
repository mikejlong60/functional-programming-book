package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import chapter5.Stream

class StreamTest extends PropSpec with PropertyChecks with Matchers {

  import Monad._
  val mon = streamMonad

  property("Test map") {
    forAll { xs: List[Int] =>
      val xxs = chapter5.Stream(xs:_*)
      val actual = mon.flatMap(xxs)(x => Stream.cons(x + 1, Stream.empty))
      val expected = xs.flatMap(x => List(x + 1))
      actual.toList should be (expected)
    }
  }

  property("Test Map Law") {
    val o = Stream.cons(1, Stream.cons(2, Stream.cons(1, Stream.empty)))
    mon.mapLaw(Stream.empty) should be (true)

    //Folliowing is same as map law but need to evaluate the stream to provew it
    val g = mon.map(o)(a => a).toList
    val h = o.toList
    g should be (h)
  }



  property("Test Associative Law") {
    forAll {xs: List[Int]  =>
      val xxs = chapter5.Stream(xs:_*)

      val f =  (x: Int) => Stream.cons(x.toString, Stream.empty)
      val g = (y: String) => Stream.cons(s"the number was: $y", Stream.empty)

      val l = mon.flatMap(mon.flatMap(xxs)(f))(g).toList
      val r = mon.flatMap(xxs)(a => mon.flatMap(f(a))(g)).toList
      l should be (r)
      mon.associativeLaw(Stream.empty)(f)(g) should be (true)
    }
  }

  /**
  property("Test Kleisli Associative Law") {
    forAll {xs: List[Int]  =>
      val f =  (xs: List[Int]) => xs.map(x => x.doubleValue)
      val g = (y: Double) => List(y.toString)
      val h =   (z: String) => List(s"the element of the list was: $z")
     mon.associativeLawUsingKleisli(xs)(f, g, h) should be (true)
    }
  }

 property("Show equivalence  of Kleisli Associative Law and flatMap associative law") {
    forAll {xs: List[Int]  =>
      val f =  (xs: List[Int]) => xs.map(x => x.doubleValue)
      val g = (y: Double) => List(y.toString)
      val h =   (z: String) => List(s"the number was: $z")
      mon.associativeLawUsingKleisli(xs)(f, g, h) should be (true)
   }
 }

  property("Prove identity laws using Kleisli composition") {
    forAll {xs: List[Int]  =>
      val f =  (xs: List[Int]) => xs.map(x => s"${x * 3}")
      mon.identityLawsUsingKleisli(xs)(f) should be (true)
    }
  }

  property("Test flatmap") {
    forAll { xs: List[Int] =>
      val actual = mon.flatMap(xs)(x => List(x + 1))
      val expected = xs.flatMap(x => List(x + 1))
      actual should be (expected)
    }
  }

  property("Test flatmap on unit value") {
    val actual = mon.flatMap(List())(x => List())
    val expected = List()
    actual should be (expected)
  }

  val f: String => List[Int] = (x: String) =>
  try {
    List(x.toInt)
  } catch {
    case e: Exception => List()
  }
  
  property("Test traverse over unparseable number") {
    val xs = List("12","13a")
    val expected =List()
    val actual = mon.traverse(xs)(f)
    actual should be (expected)
  }

  property("Test traverse") {
    forAll { xs: List[Int] =>
      val xss = xs.map(_.toString)
      val expected = List(xss.flatMap(f))
      val actual = mon.traverse(xss)(f)
      actual should be (expected)
    }
  }

  property("Test sequence for non-empty list. Sequence flattens the list by one level.") {
    forAll { l: List[Int] =>
      val ll = l.map((List(_)))
      val actual = mon.sequence(ll)
      actual should be (List(l))
    }
  }

  property("Test sequence for empty list") {
    val ll = List()
    val actual = mon.sequence(ll)
    actual should be (List(List()))
  }
    */
}

