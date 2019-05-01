package chapter8

import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks
import chapter6.SimpleRNG


class GenTest extends PropSpec with PropertyChecks with Matchers {

  property("Run choose within a range") {
    forAll{(start: Short, stopExclusive: Int) =>
      whenever (stopExclusive > start) {
        val actual = Gen.choose(start, stopExclusive)
        val rng = SimpleRNG(start)
        val fff =actual.sample.run(rng)
        fff._1 should be >= (start.toInt)
        fff._1 should be <  stopExclusive
      }
    }
  }
}


