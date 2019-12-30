package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import chapter7.nonblocking.Nonblocking.Par

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

      val a1 = mon.flatMap(mon.flatMap(mon.unit(x))(f))(g)
      val actual1 = Par.run(executor)(a1).get

      val a2 = mon.flatMap(mon.unit(x))(a => mon.flatMap(f(a))(g))
      val actual2 = Par.run(executor)(a2).get

      actual1 should be (actual2)
    }
  }

  property("Test Kleisli Associative Law") {
    forAll {x: Int  =>
      val f =  (x: Int) => mon.unit(x.toString)
      val g = (y: String) => mon.unit(s"the number was: $y")
      val h =   (z: String) => mon.unit(s"the element of the list was: $z")
      val lf = mon.compose(mon.compose(f, g), h)
      val rf = mon.compose(f, mon.compose(g, h))
      val lfe = Par.run(executor)(lf(x)).get
      val rfe = Par.run(executor)(rf(x)).get
      lfe should be (rfe)
    }
  }

  property("Prove identity laws using Kleisli composition") {
    forAll {xs: List[Int]  =>
      val a = mon.map(mon.unit(xs))(a => a.map(aa => aa + 12))
      val f =  (xs: List[Int]) => mon.map(mon.unit(xs))(a => a.map(x => s"${x * 3}"))
      val li = mon.compose(f, (b: List[String])=> mon.unit(b))
      val ri = mon.compose((a: List[Int]) => mon.unit(a), f)
      val lfe = Par.run(executor)(li(xs)).get
      val rfe = Par.run(executor)(ri(xs)).get
      lfe should be (rfe)
    }
  }

  property("Test flatmap") {
    forAll { xs: List[Int] =>
      val a = mon.flatMap(mon.unit(xs))(x => mon.unit(x.map(xx => xx + 1)))
      val actual = Par.run(executor)(a).get
      val expected = xs.flatMap(x => List(x + 1))
      actual should be (expected)
    }
  }

  property("Test flatmap that uses compose") {
    forAll { xs: List[Int] =>
      val a = mon._flatMap(mon.unit(xs))(x => mon.unit(x.map(xx => xx + 1)))
      val actual = Par.run(executor)(a).get
      val expected = xs.flatMap(x => List(x + 1))
      actual should be (expected)
    }
  }


  property("Test flatmap on unit value") {
    val a = mon.flatMap(mon.unit(List()))(x => mon.unit(List()))
    val actual = Par.run(executor)(a).get
    val expected = List()
    actual should be (expected)
  }

  val f = (x: Par[String]) =>
  //try
  {
    println("1")
    val r = Par.map(x)(xx => xx.toInt)
    println("2")
    r
  //} catch {
   // case e: Exception => {
    //  println("crap")
    //  mon.unit(e)//-1)
   // }
  }


  //TODO this blocks forever.  Figure out why.
 // property("Test traverse over unparseable number") {
 //   val xs = List("12","13a").map(x => mon.unit(x))
 //   val expected =List(-1)
  //  val a = mon.traverse(xs)(f)
  //  val actual = Par.run(executor)(a).get
    //actual should be (expected)
 // }
 
 property("Test traverse") {
    forAll { xs: List[Int] =>
      val xss = xs.map(x => mon.unit(x.toString))
      val a = mon.traverse(xss)(f)
      val actual = Par.run(executor)(a).get
      actual should be (xs)
    }
  }

  property("Test sequence for non-empty list. Sequence flattens the list by one level.") {
    forAll { l: List[Int] =>
      val ll = l.map(x => mon.unit(x))
      val a = mon.sequence(ll)
      val actual = Par.run(executor)(a).get
      actual should be (l)
    }
  }
}

