package chapter8

import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks


class PropTest extends PropSpec with PropertyChecks with Matchers {

  property("Check first property") {
    forAll{(check1: Boolean, check2: Boolean) =>
      val p = new Prop{
        def check = check1
      }
      val p2 = new Prop {
        def check = check2
      }

      val expected = new Prop {
        def check = check1 && check2
      }
      p2.&&(p).check should be (expected.check)
    }
  }
}
