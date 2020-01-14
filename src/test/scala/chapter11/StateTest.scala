package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class StateTest extends PropSpec with PropertyChecks with Matchers {

  import Monad._

  property("Test State map function for Ints") {
    val mon = stateMonad[Int]
    forAll { x: Int =>
      val s = State( (xx: Int) => (xx, xx))
      val actual = mon.map(s)((x:  Int) => x.toString).run(x)
      actual should be ((x.toString, x))
    }
  }

  property("Use map") {
    val mon = stateMonad[chapter6.RNG]
    forAll {x: Int =>
      whenever (x < 4000000) {
        val rng = chapter6.SimpleRNG(x)
        val (i, r) = rng.nextInt
        val s = State(run = (s: chapter6.RNG) => (i, r))
        val (i1,r1) = mon.map(s)((x: Int) => x + 1200).run(rng)
        val (i2,r2) = mon.map(s)((x: Int) => x + 1200).run(rng)
        val ii: (Int, chapter6.RNG) = mon.map(s)((x: Int) => x + 10).run(rng)
        val ii2: (Int, chapter6.RNG) = mon.map(s)((x: Int) => x + 11).run(rng)
        i1 should be  (i + 1200)
        val l = List(ii,ii2)
        val kk = mon.traverse(l)(p  => {
          val pp = State(run = (s: chapter6.RNG) => (p._1, p._2))
          pp
        })
        mon.map(kk)(x => {
          x
        }).run(rng)
      }
    }
  }

  property("Understand getState and setState") {
    forAll {xs: List[Int] =>
      val actual = zipWithIndex(xs)
      val expected = xs.zipWithIndex.map(x => (x._2, x._1))
      actual should be (expected)
    }
  }
}

