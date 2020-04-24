package chapter11

import scala.{Option => _, None => _, Some => _,  Right => _, Left => _, Either => _, _}
import org.scalacheck._
import Prop.{forAll, propBoolean}

object  EitherTest extends Properties("Either test") {

  import chapter4.Either
  import chapter4.Left
  import chapter4.Right

  import Monad._
  val mon = eitherMonad[List[String]]

  property("Test Either map function for Ints") =
    forAll { x: Int =>
      val actual = mon.map(Right(x))(x => x + 1)
      val expected = Right(x + 1)
      actual == expected
    }

  property("Test Either map2 function for Ints") =
    forAll { (x: Int, y: Int) =>
      val actual = mon.map2(Right(x), Right(y))((x, y) => x + y)
      val expected = Right(x + y)
      actual == expected
    }

  property("Test Either map3 function for Ints") =
    forAll { (x: Int, y: Int, z: Int) =>
      val actual = mon.map3(Right(x), Right(y), Right(z))((x, y, z) => x + y + z)
      val expected = Right(x + y + z)
      actual == expected
    }

  property("Test Map Law for Either Monad") = {
    mon.mapLaw(Right(1)) & mon.mapLaw(Left(List("heck")))
  }

  property("Test Either map function for left error") = {
    val actual = mon.map(Left(List("one")))((x:Int)  => x + 1000)
    val expected = Left(List("one"))
    actual == expected
  }


  property("Test Associative Law for Either Monad") =
    forAll {x: Int  =>
      val f =  (x: Int) => Right(x.toString)
      val g = (y: String) => Right(s"the number was: $y")

      mon.associativeLaw(Right(x))(f)(g) & mon.associativeLaw(Left(List("heck")))(f)(g)
    }

  property("Test Kleisli Associative Law for Either Monad") =
    forAll {x: Int  =>
      val f =  (x: Int) => Right(x.doubleValue)
      val g = (y: Double) => Right(y.toString)
      val h =   (z: String) => Right(s"the number was: $z")
     mon.associativeLawUsingKleisli(x)(f, g, h)
    }

  property("Show equivalence  of Kleisli Associative Law and flatMap associative law") =
    forAll {x: Int  =>
      val f =  (x: Int) => Right(x.doubleValue)
      val g = (y: Double) => Right(y.toString)
      val h =   (z: String) => Right(s"the number was: $z")
      mon.associativeLawUsingKleisli(x)(f, g, h)
    }

  property("Prove identity laws using Kleisli composition") =
    forAll {x: Int  =>
      val f =  (x: Int) => Right(x.doubleValue)
      mon.identityLawsUsingKleisli(x)(f)
    }

  property("Test Either flatmap function for Ints") =
    forAll { x: Int =>
      val actual = mon.flatMap(Right(x))(x => Right(x + 1))
      val expected = Right(x + 1)
      actual == expected
    }

  property("Test Either flatmap function for Left") =
    forAll { x: Int =>
      val actual = mon.flatMap(Left(List("heck")))(x => x)
      val expected = Left(List("heck"))
      actual == expected
    }

  property("Test Either map function for Left") = {
    val actual = mon.map(Left(List("dude")))(x => 1)
    actual == Left(List("dude"))
  }


val f: String => Either[List[String], Int] = (x: String) => 
  try {
    Right(x.toInt)
  } catch {
    case e: Exception => Left(List(s"$e"))
  }
  
  property("Test traverse over unparseable number") = {
    val xs = List("12","13a")
    val expected = Left(List("java.lang.NumberFormatException: For input string: \"13a\""))
    val actual = mon.traverse(xs)(f)
    actual == expected
  }

  property("Test traverse over empty list") = {
    val xs = Nil
    val expected = Right(Nil)
    val actual = mon.traverse(xs)(f)
    actual == expected
  }

  property("Test traverse over list of one element") = {
    val xs = List("12")
    val expected = Right(List(12))
    val actual = mon.traverse(xs)(f)
    actual == expected
  }

  property("Test sequence for non-empty list") =
    forAll { l: List[Int] =>
      val ll = l.map((Right(_)))
      val actual = mon.sequence(ll)
      actual == Right(l)
    }

  property("Test sequence for empty list") = {
    val l = List()
    val actual = mon.sequence(l)
    actual == Right(List.empty[Int])
  }
}

