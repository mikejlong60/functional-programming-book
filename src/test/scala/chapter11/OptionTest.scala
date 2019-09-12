package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scala.{Option => _, None => _, Some => _,  Either => _, _}

class OptionTest extends PropSpec with PropertyChecks with Matchers {

  import Option._
  val mon = option


  property("Test Option map function for Ints") {
    forAll { x: Int =>
      val actual = mon.map(Some(x))(x => x + 1)
      val expected = Some(x + 1)
      actual should be (expected)
    }
  }

  property("Test Map Law for Option Monad") {
    val o = Some(1)
    option.mapLaw(o) should be (true)
    option.mapLaw(None) should be (true)
  }

  property("Test Associative Law for Option Monad") {
    forAll {x: Int  =>
      val f =  (x: Int) => Some(x.toString)
      val g = (y: String) => Some(s"the number was: $y")

      mon.associativeLaw(Some(x))(f)(g) should be (true)
      mon.associativeLaw(None)(f)(g) should be (true)
    }
  }

  property("Test Kleisli Associative Law for Option Monad") {
    forAll {x: Int  =>
      val f =  (x: Int) => Some(x.doubleValue)
      val g = (y: Double) => Some(y.toString)
      val h =   (z: String) => Some(s"the number was: $z")
     mon.associativeLawUsingKleisli(x)(f, g, h) should be (true)
    }
  }

  property("Show equivalence  of Kleisli Associative Law and flatMap associative law") {
    forAll {x: Int  =>//TODO
      val f =  (x: Int) => Some(x.doubleValue)
      val g = (y: Double) => Some(y.toString)
      val h =   (z: String) => Some(s"the number was: $z")
      mon.associativeLawUsingKleisli(x)(f, g, h) should be (true)
    }
  }

  property("Prove identity laws using Kleisli composition") {
    forAll {x: Int  =>
      val f =  (x: Int) => Some(x.doubleValue)
      mon.identityLawsUsingKleisli(x)(f) should be (true)
    }
  }

  property("Test Option flatmap function for Ints") {
    forAll { x: Int =>
      val actual = mon.flatMap(Some(x))(x => Some(x + 1))
      val expected = Some(x + 1)
      actual should be (expected)
    }
  }

  property("Test Option flatmap function for None") {
    forAll { x: Int =>
      val actual = mon.flatMap(None)(x => None)
      val expected = None
      actual should be (expected)
    }
  }

  property("Test Option map function for None") {
    val actual = mon.map(None)(x => 1)
    val expected = None
    actual should be (expected)
  }


val f: String => Option[Int] = (x: String) => 
  try {
    Some(x.toInt)
  } catch {
    case e: Exception => None
  }
  
  property("Test traverse over unparseable number") {
    val xs = List("12","13a")
    val expected = None
    val actual = mon.traverse(xs)(f)
    actual should be (expected)
  }

  property("Test traverse over empty list") {
    val xs = Nil
    val expected = Some(Nil)
    val actual = mon.traverse(xs)(f)
    actual should be (expected)
  }

  property("Test traverse over list of one element") {
    val xs = List("12")
    val expected = Some(List(12))
    val actual = mon.traverse(xs)(f)
    actual should be (expected)
  }

  property("Test sequence for non-empty list") {
    forAll { l: List[Int] =>
      val ll = l.map((Some(_)))
      val actual = mon.sequence(ll)
      println(actual)
      actual should be (Some(l))
    }
  }

  property("Test sequence for empty list") {
    val l = List.empty[Option[Int]]
    val actual = mon.sequence(l)
    actual should be (Some(List.empty[Int]))
  }
}

