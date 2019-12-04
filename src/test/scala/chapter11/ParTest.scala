package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ParTest extends PropSpec with PropertyChecks with Matchers {

  import Monad._
  val mon = parMonad
  val executor = java.util.concurrent.Executors.newFixedThreadPool(8)

  import chapter7.nonblocking.Nonblocking.Par

  property("Test map") {
    forAll { xs: List[Int] =>
      val a = mon.map(mon.unit(xs))(a => a.map(aa => aa + 12))
      val actual = Par.run(executor)(a).get
      val expected = xs.map(x => x + 12)
      actual should be (expected)
    }
  }

  property("Test Map Law") {
    forAll  {xs : List[Int] =>
      val actual = Par.run(executor)(mon.map(mon.unit(xs))(a => a)).get
      actual should be (xs)
      val actual2 = Par.run(executor)(mon.map(mon.unit(List()))(a => a)).get
      actual2 should be (List())
    }
  }


  property("Test Associative Law") {
    forAll {x: Int  =>
      val f =  (x: Int) => mon.unit(x.toString)
      val g = (y: String) => mon.unit(s"the number was: $y")

      val a1 = mon.flatMap(mon.flatMap(mon.unit(x))(f))(g)// == flatMap(x)(a => flatMap(f(a))(g))
      val actual1 = Par.run(executor)(a1).get

      val a2 = mon.flatMap(mon.unit(x))(a => mon.flatMap(f(a))(g))// == flatMap(x)(a => flatMap(f(a))(g))
      val actual2 = Par.run(executor)(a2).get

      actual1 should be (actual2)
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

