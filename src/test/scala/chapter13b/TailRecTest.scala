package chapter13b

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class TailRecTest extends PropSpec with PropertyChecks with Matchers {

  property("Goof around with TailRec ") {
    forAll{ x: Short =>
      import TailRec._

      val f: Int => TailRec[Int] = (x: Int) => Return(x)

      val j: (Int) => TailRec[Int] = List.fill(x)(f).foldLeft(f) {(a: Function1[Int, TailRec [Int]], b: Function1[Int, TailRec[Int]]) => (x: Int) => TailRec.suspend(a(x).flatMap(b)) }
      (TailRec.run(j(x))) should be (x)
    }
  }
}
