package chapter11

import scala.{Option => _, None => _, Some => _,  Either => _, _}
import org.scalacheck._
import Prop.{forAll, propBoolean}

object  OptionTest extends Properties("Option test") {


  import Monad._
  val mon = optionMonad

  import chapter4.Option
  import chapter4.None
  import chapter4.Some

  property("Test replicateM") =
    forAll{  (x: Int, n: Short) =>
      val xx = Some(x)
      val actual = mon.replicateM(n, xx)
      val expected = Some(List.fill(n)(x))
      actual == expected
    }

  property("Test Option map function for Ints") =
    forAll { x: Int =>
      val actual = mon.map(Some(x))(x => x + 1)
      val expected = Some(x + 1)
      actual == expected
    }

  property("Test Map Law for Option Monad") = {
    val o = Some(1)
    mon.mapLaw(o) & mon.mapLaw(None)
  }

  property("Test Associative Law for Option Monad") =
    forAll {x: Int  =>
      val f =  (x: Int) => Some(x.toString)
      val g = (y: String) => Some(s"the number was: $y")

      mon.associativeLaw(Some(x))(f)(g) & mon.associativeLaw(None)(f)(g)
    }

  property("Test Kleisli Associative Law for Option Monad") =
    forAll {x: Int  =>
      val f =  (x: Int) => Some(x.doubleValue)
      val g = (y: Double) => Some(y.toString)
      val h =   (z: String) => Some(s"the number was: $z")
     mon.associativeLawUsingKleisli(x)(f, g, h)
    }

  property("Show equivalence  of Kleisli Associative Law and flatMap associative law") =
    forAll {x: Int  =>
      val f =  (x: Int) => Some(x.doubleValue)
      val g = (y: Double) => Some(y.toString)
      val h =   (z: String) => Some(s"the number was: $z")
      mon.associativeLawUsingKleisli(x)(f, g, h)
    }

  property("Prove identity laws using Kleisli composition") =
    forAll {x: Int  =>
      val f =  (x: Int) => Some(x.doubleValue)
      mon.identityLawsUsingKleisli(x)(f)
    }

  property("Test Option flatmap function for Ints") =
    forAll { x: Int =>
      val actual = mon.flatMap(Some(x))(x => Some(x + 1))
      val expected = Some(x + 1)
      actual == expected
    }

  property("Test Option flatmap function for None") =
    forAll { x: Int =>
      val actual = mon.flatMap(None)(x => None)
      val expected = None
      actual == expected
    }

  property("Test Option map function for None") = {
    val actual = mon.map(None)(x => 1)
    val expected = None
    actual == expected
  }


  val f: String => Option[Int] = (x: String) =>
    try {
      Some(x.toInt)
    } catch {
      case e: Exception => None
    }
  
  property("Test traverse over unparseable number") = {
    val xs = List("12","13a")
    val expected = None
    val actual = mon.traverse(xs)(f)
    actual == expected
  }

  property("Test traverse over empty list") = {
    val xs = Nil
    val expected = Some(Nil)
    val actual = mon.traverse(xs)(f)
    actual == expected
  }

  property("Test traverse over list of one element") = {
    val xs = List("12")
    val expected = Some(List(12))
    val actual = mon.traverse(xs)(f)
    actual == expected
  }

  property("Test sequence for non-empty list") =
    forAll { l: List[Int] =>
      val ll = l.map((Some(_)))
      val actual = mon.sequence(ll)
      actual == Some(l)
    }

  property("Test sequence for empty list") = {
    val l = List.empty[Option[Int]]
    val actual = mon.sequence(l)
    actual == Some(List.empty[Int])
  }
}

