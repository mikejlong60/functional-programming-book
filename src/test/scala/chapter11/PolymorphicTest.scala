package chapter11

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class PolymorphicTest extends PropSpec with PropertyChecks with Matchers {

  import Monad._

  def filterMTest[F[_]](mon: Monad[F]) = {
    forAll{  (xs: List[Int]) =>
      println(xs)
      val f =  (a: Int) => if (a > 1000) mon.unit(true) else mon.unit(false)
      val actual = mon.filterM(xs)(f)
      val expected = xs.filter(x => x > 1000)
      mon.map(actual)(a => a should be (expected))
    }
  }
  
  property("Test filterM") {
    filterMTest(listMonad)
    filterMTest(optionMonad)
    filterMTest(streamMonad)
    filterMTest(parMonad)
    filterMTest(IOMonad)
    //filterMTest(eitherMonad[List[Int]])
    //filterMTest(stateMonad[Int])
  }
}
