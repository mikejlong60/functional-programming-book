package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class IdTest extends PropSpec with PropertyChecks with Matchers {

  import Monad._
  val mon = idMonad

  property("Test replicateM") {
    forAll{  (x: Int, n: Short) =>
      val xx = Id(x)
      val actual = mon.replicateM(n, xx)
      val expected = Id(List.fill(n)(x))
      actual should be (expected)
    }
  }

  property("Test map function for Ints") {
    forAll { x: Int =>
      val actual = mon.map(Id(x))(x => x + 1)
      val expected = Id(x + 1)
      actual should be (expected)
    }
  }

  property("Test Map Law") {
    val o = Id(1)
    mon.mapLaw(o) should be (true)
  }

  property("Test Associative Law") {
    forAll {x: Int  =>
      val f =  (x: Int) => Id(x.toString)
      val g = (y: String) => Id(s"the number was: $y")

      mon.associativeLaw(Id(x))(f)(g) should be (true)
    }
  }

  property("Test Kleisli Associative Law") {
    forAll {x: Int  =>
      val f =  (x: Int) => Id(x.doubleValue)
      val g = (y: Double) => Id(y.toString)
      val h =   (z: String) => Id(s"the number was: $z")
     mon.associativeLawUsingKleisli(x)(f, g, h) should be (true)
    }
  }

  property("Show equivalence  of Kleisli Associative Law and flatMap associative law") {
    forAll {x: Int  =>
      val f =  (x: Int) => Id(x.doubleValue)
      val g = (y: Double) => Id(y.toString)
      val h =   (z: String) => Id(s"the number was: $z")
      mon.associativeLawUsingKleisli(x)(f, g, h) should be (true)
    }
  }

  property("Prove identity laws using Kleisli composition") {
    forAll {x: Int  =>
      val f =  (x: Int) => Id(x.doubleValue)
      mon.identityLawsUsingKleisli(x)(f) should be (true)
    }
  }

  property("Test  flatmap function for Ints") {
    forAll { x: Int =>
      val actual = mon.flatMap(Id(x))(x => Id(x + 1))
      val expected = Id(x + 1)
      actual should be (expected)
    }
  }

val f: String => Id[Int] = (x: String) => 
  try {
    Id(x.toInt)
  } catch {
    case e: Exception => Id(-1)
  }
  
  property("Test traverse over unparseable number") {
    val xs = List("12","13a")
    val expected = Id(List(12, -1))
    val actual = mon.traverse(xs)(f)
    actual should be (expected)
  }

  property("Test traverse over empty list") {
    val xs = Nil
    val expected = Id(Nil)
    val actual = mon.traverse(xs)(f)
    actual should be (expected)
  }

  property("Test traverse over list of one element") {
    val xs = List("12")
    val expected = Id(List(12))
    val actual = mon.traverse(xs)(f)
    actual should be (expected)
  }

  property("Test sequence for non-empty list") {
    forAll { l: List[Int] =>
      val ll = l.map((Id(_)))
      val actual = mon.sequence(ll)
      actual should be (Id(l))
    }
  }

  property("Test sequence for empty list") {
    val l = List.empty[Id[Int]]
    val actual = mon.sequence(l)
    actual should be (Id(List.empty[Int]))
  }
}

