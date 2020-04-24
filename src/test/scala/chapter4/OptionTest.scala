package chapter4

import scala.{List => _, Option => _, None => _, Some => _,  Either => _, _}
import chapter3.Cons
import chapter3.Nil
import scala.{List => _, Option => _, None => _, Some => _,  Either => _, _}
import org.scalacheck._
import Prop.{forAll, propBoolean}

object OptionTest extends Properties("Option tests") {

  property("Test Option map function for Ints") =
    forAll { x: Int =>
      val actual = Some(x).map(x => x + 1)
      val expected = Some(x + 1)
      actual == expected
  }

  property("Test Option flatmap function for Ints")  =
    forAll { x: Int =>
      val actual = Some(x).flatMap(x => Some(x + 1))
      val expected = Some(x + 1)
      actual == expected
    }

  property("Test Option flatmap function for None") =
    forAll { x: Int =>
      val actual = None.flatMap(x => None)
      val expected = None
      actual == expected
    }

  property("Test Option map function for None") = {
      val actual = None.map(x => 1)
      val expected = None
      actual == expected
  }

  property("Test Option getOrElse function for None") =
    forAll { x: Int =>
      val actual = None.getOrElse(12)
      val expected = 12
      actual == expected
    }

  property("Test Option getOrElse function for Int") =
    forAll { x: Int =>
      val actual = Some(x).getOrElse(12)
      val expected = x
      actual == expected
    }

  property("Test Option orElse function for None") =
    forAll { x: Int =>
      val actual = None.orElse(Some(12))
      val expected = Some(12)
      actual == expected
    }

  property("Test Option orElse function for Int") =
    forAll { x: Int =>
      val actual = Some(x).orElse(Some(12))
      val expected = Some(x)
      actual == expected
    }

  property("Test Option filter function for None") =
    forAll { x: Int =>
      val actual = None.filter(x => true)
      val expected = None
      actual == expected
    }

  property("Test Option filter function for Int") =
    forAll { x: Int =>
      val actual = Some(x).filter(y => y == x)
      val expected = Some(x)
      actual == expected
    }

  property("Test Option not filter function for Int") =
    forAll { x: Int =>
      val actual = Some(x).filter(y => y != x)
      val expected = None
      actual == expected
    }

  property("Test lift") =
    forAll {x: Int =>
       val f = Option.lift(math.abs)
      val actual = f(Some(x))
      val g = (x:Int => Int) => x
      val expected = Some(math.abs(x))
      actual == expected
    }

  property("Test map2") =
    forAll {(x: Int, y: Int) =>
      val f = (x: Int, y: Int) => math.abs(x) + math.abs(y)
      val a = Some(x)
      val b = Some(y)
      val what = Some(12)
      val actual =  Option.map2(a, b)(f)
      val expected = Some(f(x, y))
      actual == expected
    }

  property("Test sequence over non-empty list") = {
    val xs = Cons(Some(12), Cons(Some(13), Nil))
    val expected = Some(Cons(12, Cons(13, Nil)))
    val actual = Option.sequence(xs)
    actual == expected
  }

  property("Test sequence over empty list") = {
    val xs = Nil
    val expected = None
    val actual = Option.sequence(xs)
    actual == expected
  }

  property("Test sequence over list of one element") = {
    val xs = Cons(Some(12), Nil)
    val expected = Some(Cons(12, Nil))
    val actual = Option.sequence(xs)
    actual == expected
  }

  val f: String => Option[Int] = (x: String) => 
  try {
    Some(x.toInt)
  } catch {
    case e: Exception => None
  }
  
  property("Test traverse over unparseable number") = {
    val xs = Cons("12", Cons("13a", Nil))
    val expected = None
    val actual = Option.traverse(xs)(f)
    actual == expected
  }

  property("Test traverse over empty list") = {
    val xs = Nil
    val expected = Some(Nil)
    val actual = Option.traverse(xs)(f)
    actual == expected
  }

  property("Test traverse over list of one element") = {
    val xs = Cons("12", Nil)
    val expected = Some(Cons(12, Nil))
    val actual = Option.traverse(xs)(f)
    actual == expected
  }
}

