package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scala.{List => _, Option => _, None => _, Some => _,  Either => _, _}
import chapter3.List
import chapter3.Cons
import chapter3.Nil


class OptionTest extends PropSpec with PropertyChecks with Matchers {

  import Option._
  val mon = option


  property("Test Option map function for Ints") {
    forAll { x: Int =>
      val actual = mon.map(Some(x))(x => x + 1)
      val expected = Some(x + 1)
      actual should be (expected)
    }
  }

  property("Test Option flatmap function for Ints") {
    forAll { x: Int =>
      val actual = mon.flatMap(Some(x))(x => Some(x + 1))
      val expected = Some(x + 1)
      actual should be (expected)
    }
  }

  property("Test Option flatmap function for None") {
    forAll { x: Int =>
      val actual = mon.flatMap(None)(x => None)
      val expected = None
      actual should be (expected)
    }
  }

  property("Test Option map function for None") {
    val actual = mon.map(None)(x => 1)
    val expected = None
    actual should be (expected)
  }


//  val f: String => Option[Int] = (x: String) => 
//  try {
//    Some(x.toInt)
//  } catch {
//    case e: Exception => None
//  }
  
//  property("Test traverse over unparseable number") {
//    val xs = Cons("12", Cons("13a", Nil))
//    val expected = None
//    val actual = Option.traverse(xs)(f)
//    actual should be (expected)
//  }

//  property("Test traverse over empty list") {
//    val xs = Nil
//    val expected = Some(Nil)
//    val actual = Option.traverse(xs)(f)
//.    actual should be (expected)
//  }

//  property("Test traverse over list of one element") {
////    val xs = Cons("12", Nil)
//    val expected = Some(Cons(12, Nil))
//    val actual = Option.traverse(xs)(f)
//    actual should be (expected)
//  }
}

