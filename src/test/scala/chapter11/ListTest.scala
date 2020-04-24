package chapter11

import org.scalacheck._
import Prop.{forAll, propBoolean}

object  ListTest extends Properties("List test") {

  import Monad._
  val mon = listMonad

  property("Test replicateM") =
    forAll{  (xx: List[Int], n: Short) =>
      val x = List(1,2,3)
      val nn = 3
      val actual = mon.replicateM(nn, x)
      val expected = List.fill(nn)(x)
      actual == expected
    }

  property("Test map") =
    forAll { x: List[Int] =>
      val actual = mon.flatMap(x)(x => List(x + 1))
      val expected = x.flatMap(x => List(x + 1))
      actual == expected
    }

  property("Test Map Law") = {
    val o = List(1,2,3)
    mon.mapLaw(o) & mon.mapLaw(List())
  }

  property("Test Associative Law") =
    forAll {xs: List[Int]  =>
      val f =  (x: List[Int]) => List(x.toString)
      val g = (y: String) => List(s"the number was: $y")

      mon.associativeLaw(List(xs))(f)(g) & mon.associativeLaw(List())(f)(g)
    }

  property("Test Kleisli Associative Law") =
    forAll {xs: List[Int]  =>
      val f =  (xs: List[Int]) => xs.map(x => x.doubleValue)
      val g = (y: Double) => List(y.toString)
      val h =   (z: String) => List(s"the element of the list was: $z")
     mon.associativeLawUsingKleisli(xs)(f, g, h)
    }

 property("Show equivalence  of Kleisli Associative Law and flatMap associative law") =
    forAll {xs: List[Int]  =>
      val f =  (xs: List[Int]) => xs.map(x => x.doubleValue)
      val g = (y: Double) => List(y.toString)
      val h =   (z: String) => List(s"the number was: $z")
      mon.associativeLawUsingKleisli(xs)(f, g, h)
    }


  property("Prove identity laws using Kleisli composition") =
    forAll {xs: List[Int]  =>
      val f =  (xs: List[Int]) => xs.map(x => s"${x * 3}")
      mon.identityLawsUsingKleisli(xs)(f)
    }

  property("Test flatmap") =
    forAll { xs: List[Int] =>
      val actual = mon.flatMap(xs)(x => List(x + 1))
      val expected = xs.flatMap(x => List(x + 1))
      actual == expected
    }

  property("Test flatmap on unit value") = {
    val actual = mon.flatMap(List())(x => List())
    val expected = List()
    actual == expected
  }

  val f: String => List[Int] = (x: String) =>
  try {
    List(x.toInt)
  } catch {
    case e: Exception => List()
  }
  
  property("Test traverse over unparseable number") = {
    val xs = List("12","13a")
    val expected =List()
    val actual = mon.traverse(xs)(f)
    actual == expected
  }

  property("Test traverse") =
    forAll { xs: List[Int] =>
      val xss = xs.map(_.toString)
      val expected = List(xss.flatMap(f))
      val actual = mon.traverse(xss)(f)
      actual == expected
    }

  property("Test sequence for non-empty list. Sequence flattens the list by one level.") =
    forAll { l: List[Int] =>
      val ll = l.map((List(_)))
      val actual = mon.sequence(ll)
      actual == List(l)
    }

  property("Test sequence for empty list") = {
    val ll = List()
    val actual = mon.sequence(ll)
    actual == List(List())
  }
}

