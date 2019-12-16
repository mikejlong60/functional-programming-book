package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ListTest extends PropSpec with PropertyChecks with Matchers {

  import Monad._
  val mon = listMonad

  property("Test replicateM") {
    forAll{  (xX: List[Int], nn: Int) =>
      val x = List(1,2,3)
      val n = 3
      val actual = mon.replicateM(n, x)
      val expected = List.fill(n)(x)
      actual should be (expected)
    }
  }
  property("Test map") {
    forAll { x: List[Int] =>
      val actual = mon.flatMap(x)(x => List(x + 1))
      val expected = x.flatMap(x => List(x + 1))
      actual should be (expected)
    }
  }

  property("Test Map Law") {
    val o = List(1,2,3)
    mon.mapLaw(o) should be (true)
    mon.mapLaw(List()) should be (true)
  }

  property("Test Associative Law") {
    forAll {xs: List[Int]  =>
      val f =  (x: List[Int]) => List(x.toString)
      val g = (y: String) => List(s"the number was: $y")

      mon.associativeLaw(List(xs))(f)(g) should be (true)
      mon.associativeLaw(List())(f)(g) should be (true)
    }
  }

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
}

