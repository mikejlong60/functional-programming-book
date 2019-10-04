package chapter2

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class CurryTest extends PropSpec with PropertyChecks with Matchers {

  def curry3[A, B, C, D](f: (A, B, C) => D): A => (B => (C => D)) = a => b => c => f(a, b, c)

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def uncurry3[A, B, C, D](f: A => B => C => D): (A, B, C) => D = (a: A, b: B, c: C) => f(a)(b)(c)

  property("Test curry and uncurry function with a Short and a Long") {

    forAll { (n: Short, o: Long, p: Long) =>
      val cat: (Short, Long, Long) => String = (x: Short, y: Long, z: Long) => s"$x : $y : $z"
      val curried: (Short) => (Long) => (Long) => String = curry3(cat)
      val uncurried: (Short, Long, Long) => String = uncurry3(curried)
      val res: String = uncurried(n, o, p)
      (curried(n)(o)(p)) should be(uncurried(n, o, p))
    }
  }

  property("Test curry and uncurry function with a Short array and a Long array") {
    forAll { (n: Array[Short], o: Array[Long], p: Array[Long]) =>
      val cat: (Array[Short], Array[Long], Array[Long]) => String = (x: Array[Short], y: Array[Long], z: Array[Long]) => s"${x.mkString(",")} : ${y.mkString(",")} : ${z.mkString(",")}"
      val curried: (Array[Short]) => (Array[Long]) => (Array[Long]) => String = curry3(cat)
      val uncurried: (Array[Short], Array[Long], Array[Long]) => String = uncurry3(curried)
      (curried(n)(o)(p)) should be(uncurried(n, o, p))
    }
  }
}

