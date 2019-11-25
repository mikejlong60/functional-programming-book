package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ListTest extends PropSpec with PropertyChecks with Matchers {

  import Monad._
  val mon = listMonad

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

//  property("Prove identity laws using Kleisli composition") {
//    forAll {x: Int  =>
//      val f =  (x: Int) => Some(x.doubleValue)
//      mon.identityLawsUsingKleisli(x)(f) should be (true)
//    }
 // /}
//
//  property("Test Option flatmap function for Ints") {
//    forAll { x: Int =>
//      val actual = mon.flatMap(Some(x))(x => Some(x + 1))
//      val expected = Some(x + 1)
//      actual should be (expected)
//    }
//  }

//  property("Test Option flatmap function for None") {
//    forAll { x: Int =>
//      val actual = mon.flatMap(None)(x => None)
//      val expected = None
//      actual should be (expected)
//    }
//  }

  property("Test Option map function for List of Ints") {
    forAll{xs: List[Int] =>
      val actual = mon.map(xs)(x => x + 1)
      val expected = xs.map(x =>x + 1)
      actual should be (expected)
    }
  }


val f: String => Option[Int] = (x: String) => 
  try {
    Some(x.toInt)
  } catch {
    case e: Exception => None
  }
  
  //  property("Test traverse over unparseable number") {
  //    val xs = List("12","13a")
 //   val expected = None
  //  val actual = mon.traverse(xs)(f)
 ///   actual should be (expected)
 // }

//  property("Test traverse over empty list") {
//    val xs = Nil
//    val expected = Some(Nil)
//    val actual = mon.traverse(xs)(f)
//    actual should be (expected)
//  }

//  property("Test traverse over list of one element") {
//    val xs = List("12")
//    val expected = Some(List(12))
//    val actual = mon.traverse(xs)(f)
//    actual should be (expected)
//  }

//  property("Test sequence for non-empty list") {
//    forAll { l: List[Int] =>
//      val ll = l.map((Some(_)))
//      val actual = mon.sequence(ll)
//      actual should be (Some(l))
//    }
//  }

//  property("Test sequence for empty list") {
//    val l = List.empty[Option[Int]]
//    val actual = mon.sequence(l)
//    actual should be (Some(List.empty[Int]))
//  }
}

