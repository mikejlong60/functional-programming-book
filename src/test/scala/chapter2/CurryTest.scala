package chapter2

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class CurryTest extends PropSpec with PropertyChecks with Matchers {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  property("Test curry function with a Short and a Long") {

    forAll { (n: Short, o: Long) =>
      val cat: (Short, Long) => String = (x: Short, y: Long) => s"$x : $y"
      val actual = curry(cat)
      val fff = actual.apply(n).apply(o)
      fff should be(s"$n : $o")
    }
  }

  property("Test curry function with a Short array and a Long array") {
    forAll { (n: Array[Short], o: Array[Long]) =>
      val cat: (Array[Short], Array[Long]) => String = (x: Array[Short], y: Array[Long]) => s"${x.mkString(",")} : ${y.mkString(",")}"
      val actual = curry(cat)
      val fff = actual.apply(n).apply(o)
      println(fff)
      fff should be(s"${n.mkString(",")} : ${o.mkString(",")}")
    }
  }


}

