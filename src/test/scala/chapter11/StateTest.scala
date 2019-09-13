package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
//import scala.{Option => _, None => _, Some => _,  Right => _, Left => _, Either => _, _}

class StateTest extends PropSpec with PropertyChecks with Matchers {

  import Monad._
  val mon = stateMonad[Int]

  val monC6 = stateMonad[chapter6.RNG]

  property("Test State map function for Ints") {
    forAll { x: Int =>
      val s = State( (xx: Int) => (xx, xx))
      val actual = mon.map(s)((x:  Int) => x.toString).run(x)
      actual should be ((x.toString, x))
    }
  }

  property("Use map") {
    forAll {x: Int =>
      whenever (x < 4000000) {
      val rng = chapter6.SimpleRNG(x)
      val (i, r) = rng.nextInt
      val s = State(run = (s: chapter6.RNG) => (i, r))
      val (i2,r2) = monC6.map(s)((x: Int) => x + 1200).run(rng)
        i2 should be  (i + 1200)
        println(r2)//r2 should be (true)
      }
    }
  }


}

