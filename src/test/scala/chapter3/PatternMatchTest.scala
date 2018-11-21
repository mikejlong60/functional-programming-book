package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import List._

class PatternMatchTest extends PropSpec with PropertyChecks with Matchers {

  property("Pattern Match understanding") {

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    x should be(3)
  }
}

