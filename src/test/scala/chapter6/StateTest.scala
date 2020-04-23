package chapter6

//import org.scalatest.prop.PropertyChecks
//import org.scalatest.{Matchers, PropSpec}
//import org.scalactic.TypeCheckedTripleEquals._
import State._

//class StateTest extends PropSpec with PropertyChecks with Matchers {
  import org.scalacheck._
  import Prop.{forAll, propBoolean}

  object StateTest extends Properties("State tests") {

  property("Generating two random numbers using the same generator produces the same number ") =
    forAll { x: Int =>
      val rng = SimpleRNG(x)
      val (n1, rng2) = rng.nextInt
      val (n2, rng3) = rng.nextInt
      n1 == n2
      rng != rng3
    }

  property("Generate a non-negative random number") =
    forAll{x: Int =>
      val rng = SimpleRNG(x)
      val (n1, rng2) = nonNegativeInt(rng: RNG)
      n1  >= 0
      rng2 != rng
    }

  property("Generate a non-negative double between 0 and 1") =
    forAll{x: Int =>
      val rng = SimpleRNG(x)
      val actual = double(rng)
      val (i, r) = actual.run(rng)
      r != (rng)
      i.toInt ==  0
    }

  property("Use map") =
    forAll {x: Int =>
      (x < 4000000) ==> {
      val rng = SimpleRNG(x)
      val (i, r) = rng.nextInt
      val s = State(run = (s: RNG) => (i, r))
      val (i2,r2) = s.map(x => x + 1200).run(rng)
      i2 ==  (i + 1200)
      }
    }

  property("Use flatMap") =
    forAll {x: Int =>
      (x < 4000000) ==> {
      val rng = SimpleRNG(x)
      val (i, r) = rng.nextInt
      val s = State(run = (s: RNG) => (i, r))
      val (i2,r2) = s.flatMap(x => unit(x + 1200)).run(rng)
      i2 ==  (i + 1200)
      }
    }

  property("Chain map and FlatMap") =
    forAll {x: Int =>
      (x < 4000000) ==> {
      val rng = SimpleRNG(x)
      val (i, r) = rng.nextInt
      val s = State(run = (s: RNG) => (i, r))
      val (i2,r2) = s.map(x => x + 1200).flatMap(x => unit(x + 1200)).flatMap(x => unit(x + 10)).run(rng)
      i2 ==  (i + 2400 + 10)
      }
    }
}
