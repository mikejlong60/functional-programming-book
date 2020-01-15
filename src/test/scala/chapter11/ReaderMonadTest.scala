package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scala.{Option => _, None => _, Some => _,  Right => _, Left => _, Either => _, _}

class ReaderMonadTest extends PropSpec with PropertyChecks with Matchers {

  val mon = chapter11.Monad.readerMonad[Int]
  val f1 = Reader((x:Int)  => x.toLong)
  val f2 = (x: Long) => x * 1000
  val f3 = Reader(f2)

  property("Test map function") {
    forAll { x: Short =>
      val actual = mon.map(f1)(f2).run(x)
      val expected = (x * 1000).toLong
      actual should be (expected)
    }
  }

  property("Test map2 function") {
    forAll { (x: Short) =>
      val actual = mon.map2(f1, f1)((x, y) => x + y).run(x)
      val expected = x * 2
      actual should be (expected)
    }
  }


  property("Test map3 function ") {
    forAll { (x: Short) =>
      val r1 = Reader((x:Int)  => x.toLong  - 3)
      val r2 = Reader((x:Int)  => x.toLong + 3)
      val r3 = Reader((x:Int)  => x.toLong + 30)

      val actual = mon.map3(r1,r2,r3)((x, y, z) => x + y + z).run(x)
      val expected = (x - 3) + (x + 3) + (x + 30)
      actual should be (expected)
    }
  }

  property("Test Map Law") {
    forAll{x: Int =>
      val actual = mon.map(f1)(a => a).run(x)
      actual should be (x)
    }
  }

  property("Test Associative Law") {
    forAll { (x: Int) =>
      val m = Reader((x:Int)  => x  - 3)
      val f = (x:Int)  => mon.unit(x + 3)
      val g = (x:Int)  => mon.unit(x + 30)

      val left = mon.flatMap(mon.flatMap(m)(f))(g).run(x)
      val right = mon.flatMap(m)(a => mon.flatMap(f(a))(g)).run(x)
      left should be (right)
    }
  }

  property("Test Kleisli Associative Law") {
    forAll {x: Int  =>
     val m = Reader((x:Int)  => x  - 3)
      val f = (x:Int)  => mon.unit(x + 3)
      val g = (x:Int)  => mon.unit(x + 30)
      val h = (x:Int)  => mon.unit(x + 300)

      val lf = mon.compose(mon.compose(f, g),h)
      val rf = mon.compose(f,(mon.compose(g,h)))
      val left = lf(x).run(x)
      val right = rf(x).run(x)
      left should be(right)
    }
  }
/**
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

  property("Test Either flatmap function for Ints") {
    forAll { x: Int =>
      val actual = mon.flatMap(Right(x))(x => Right(x + 1))
      val expected = Right(x + 1)
      actual should be (expected)
    }
  }

  property("Test Either flatmap function for Left") {
    forAll { x: Int =>
      val actual = mon.flatMap(Left(List("heck")))(x => x)
      val expected = Left(List("heck"))
      actual should be (expected)
    }
  }

  property("Test Either map function for Left") {
    val actual = mon.map(Left(List("dude")))(x => 1)
    val expected = Left
    actual should be (Left(List("dude")))
  }


val f: String => Either[List[String], Int] = (x: String) => 
  try {
    Right(x.toInt)
  } catch {
    case e: Exception => Left(List(s"$e"))
  }
  
  property("Test traverse over unparseable number") {
    val xs = List("12","13a")
    val expected = Left(List("java.lang.NumberFormatException: For input string: \"13a\""))
    val actual = mon.traverse(xs)(f)
    actual should be (expected)
  }

  property("Test traverse over empty list") {
    val xs = Nil
    val expected = Right(Nil)
    val actual = mon.traverse(xs)(f)
    actual should be (expected)
  }

  property("Test traverse over list of one element") {
    val xs = List("12")
    val expected = Right(List(12))
    val actual = mon.traverse(xs)(f)
    actual should be (expected)
  }

  property("Test sequence for non-empty list") {
    forAll { l: List[Int] =>
      val ll = l.map((Right(_)))
      val actual = mon.sequence(ll)
      actual should be (Right(l))
    }
  }

  property("Test sequence for empty list") {
    val l = List()
    val actual = mon.sequence(l)
    actual should be (Right(List.empty[Int]))
  }

  */
}

