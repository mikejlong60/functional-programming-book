package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
//import scala.{Option => _, None => _, Some => _,  Right => _, Left => _, Either => _, _}

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
        println(r1)//r2 should be (true)



        val l = List(ii,ii2)//s, s)//(i1, r1), (i2, r2))
        val kk = mon.traverse(l)(p  => {
          println("p:"+p)
          val pp = State(run = (s: chapter6.RNG) => (p._1, p._2))
          println("pp"+pp)
          pp
        })
        println("dude:"+kk)
        mon.map(kk)(x => {
          println("x:"+x)
          x
        }).run(rng)
      }
    }
  }


}

