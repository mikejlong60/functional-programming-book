package chapter2

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class CurryTest extends PropSpec with PropertyChecks with Matchers {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  property("Test curry and uncurry function with a Short and a Long") {

    forAll { (n: Short, o: Long) =>
      val cat: (Short, Long) => String = (x: Short, y: Long) => s"$x : $y"
      val curried = curry(cat)
      val uncurried = uncurry(curried)
      val res = uncurried(n, o)
      println("curried:" + curried(n)(o))
      println("uncurried:" +uncurried(n, o))
      (curried(n)(o)) should be(uncurried(n, o))
    }
  }

  property("Test curry and uncurry function with a Short array and a Long array") {
    forAll { (n: Array[Short], o: Array[Long]) =>
      val cat: (Array[Short], Array[Long]) => String = (x: Array[Short], y: Array[Long]) => s"${x.mkString(",")} : ${y.mkString(",")}"
      val curried = curry(cat)
      val uncurried = uncurry(curried)
      println("curried:" + curried(n)(o))
      println("uncurried:" +uncurried(n, o))
      (curried(n)(o)) should be(uncurried(n, o))
    }
  }
}

