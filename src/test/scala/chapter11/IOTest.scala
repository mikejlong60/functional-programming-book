package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scala.{Option => _, None => _, Some => _,  Either => _, _}

class IOTest extends PropSpec with PropertyChecks with Matchers {

  import Monad._
  val mon = IOMonad

  val f: () => Int = () =>  {
    Thread.sleep(10)//Big expensive computation that changes something in outside world.
    12
  }


  property("Test Map Law for IO Monad") {
    val o = mon.unit(f)
    val l = o.run()
    val r = mon.map(o)(f => f).run()
    l should be (r)
  }

    property("Test Associative Law for IO Monad") {
      forAll {x: Int  =>
        val g =  (x: Int) => mon.unit(x.toString)
        val h = (y: String) => mon.unit(s"the number was: $y")
        val x1 = mon.unit(x)
        val l = mon.flatMap(mon.flatMap(x1)(g))(h).run
        val r =  mon.flatMap(x1)(a => mon.flatMap(g(a))(h)).run
        l should be (r)
      }
    }

  property("Prove IO identity law using Kleisli composition") {
    forAll {a: Int  =>
      val f =  (x: Int) => mon.unit(x.doubleValue)
      val li = mon.compose(f, (a: Double)=> mon.unit(a))
      val ri = mon.compose((a: Int)=> mon.unit(a), f)
      println(li(a).run)
      println(ri(a).run)
      li(a).run == ri(a).run
      li(a) == ri(a)
    }
  }
}

