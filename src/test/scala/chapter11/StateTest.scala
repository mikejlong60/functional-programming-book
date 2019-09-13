package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
//import scala.{Option => _, None => _, Some => _,  Right => _, Left => _, Either => _, _}

class StateTest extends PropSpec with PropertyChecks with Matchers {

  import Monad._
  val mon = stateMonad[Int]

  property("Test State map function for Ints") {
    forAll { x: Int =>
      val s = State( (xx: Int) => (xx, xx))
      val actual = mon.map(s)((x:  Int) => x.toString).run(x)
      actual should be ((x.toString, x))
    }
  }
}

