package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scala.{Option => _, None => _, Some => _,  Right => _, Left => _, Either => _, _}

class EitherTest extends PropSpec with PropertyChecks with Matchers {

  import Monad._

  import chapter4.Either
  import chapter4.Left
  import chapter4.Right

  import Monad._
  val mon = eitherMonad

  property("Test Either map function for Ints") {
    forAll { x: Int =>
      val actual = mon.map(Right(x))(x => x + 1)
      val expected = Right(x + 1)
      actual should be (expected)
    }
  }

  property("Test Map Law for Option Monad") {
    val o = Right(1)
    mon.mapLaw(o) should be (true)
    //mon.mapLaw(Left(1)) should be (true)
  }

  property("Test Either map function for left error") {
    forAll { x: String =>
     // val actual = mon.map(Left(x))(x => x + "hi")
      //val expected = chapter4.Left(x + 1)
      //actual should be (expected)
    }
  }

  ////////////
  //////////
    property("Test Associative Law for Option Monad") {
    forAll {x: Int  =>
      val f =  (x: Int) => Right(x.toString)
      val g = (y: String) => Right(s"the number was: $y")

      mon.associativeLaw(Right(x))(f)(g) should be (true)
      //mon.associativeLaw(Left(""))(f)(g) should be (true)
    }
  }

  property("Test Kleisli Associative Law for Option Monad") {
    forAll {x: Int  =>
      val f =  (x: Int) => Right(x.doubleValue)
      val g = (y: Double) => Right(y.toString)
      val h =   (z: String) => Right(s"the number was: $z")
     mon.associativeLawUsingKleisli(x)(f, g, h) should be (true)
    }
  }

  property("Show equivalence  of Kleisli Associative Law and flatMap associative law") {
    forAll {x: Int  =>
      val f =  (x: Int) => Right(x.doubleValue)
      val g = (y: Double) => Right(y.toString)
      val h =   (z: String) => Right(s"the number was: $z")
      mon.associativeLawUsingKleisli(x)(f, g, h) should be (true)
    }
  }

  property("Prove identity laws using Kleisli composition") {
    forAll {x: Int  =>
      val f =  (x: Int) => Right(x.doubleValue)
      mon.identityLawsUsingKleisli(x)(f) should be (true)
    }
  }

  property("Test Option flatmap function for Ints") {
    forAll { x: Int =>
      val actual = mon.flatMap(Right(x))(x => Right(x + 1))
      val expected = Right(x + 1)
      actual should be (expected)
    }
  }

  property("Test Option flatmap function for None") {
    forAll { x: Int =>
      //val actual = mon.flatMap(Left)(x => None)
      //val expected = None
      //actual should be (expected)
    }
  }

  property("Test Option map function for None") {
    //val actual = mon.map(Left)(x => 1)
    //val expected = Left
    //actual should be (expected)
  }


val f: String => Either[String, Int] = (x: String) => 
//val f  = (x: String) => 
  try {
    Right(x.toInt)
  } catch {
    case e: Exception => Left(e.getMessage)
  }
  
  property("Test traverse over unparseable number") {
    val xs = List("12","13a")
    //val expected = Left("dd")
   // val actual = mon.traverse(xs)(f)
    //actual should be (expected)
  }

  property("Test traverse over empty list") {
    val xs = Nil
    val expected = Right(Nil)
    //val actual = mon.traverse(xs)(f)
    //actual should be (expected)
  }

  property("Test traverse over list of one element") {
    val xs = List("12")
    val expected = Right(List(12))
    //val actual = mon.traverse(xs)(f)
    //actual should be (expected)
  }

  property("Test sequence for non-empty list") {
    forAll { l: List[Int] =>
      val ll = l.map((Right(_)))
      val actual = mon.sequence(ll)
      println(actual)
      actual should be (Right(l))
    }
  }

  property("Test sequence for empty list") {
    val l = List()
    val actual = mon.sequence(l)
    actual should be (Right(List.empty[Int]))
  }
}

