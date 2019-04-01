package chapter7

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.scalactic.TypeCheckedTripleEquals._ 
import Par._

class ParTest extends PropSpec with PropertyChecks with Matchers {

  property("Sum in parallel not ") {
    forAll { xs: List[Int] => {
      val actual = sumInParallelNot(xs)
      val expected = xs.sum
      actual should be (expected)
    }
   }
  }
}
