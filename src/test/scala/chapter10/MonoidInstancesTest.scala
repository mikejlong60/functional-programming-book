package chapter10

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import MonoidInstances._

class MonoidInstancesTest extends PropSpec with PropertyChecks with Matchers {

  property("String Monoid op") {
    forAll{(x: String, y: String) =>
      val actual = stringMonoid.op(x, y)
      actual should be (x + y)
    }
  }

  property("String Monoid zero") {
    forAll{ x: String =>
      val actual = stringMonoid.op(stringMonoid.zero, x)
      actual should be (x)
    }
  }

  property("Int Addition Monoid op") {
    forAll{(x: Int, y: Int) =>
      val actual = intAddition.op(x, y)
      actual should be (x + y)
    }
  }

  property("Int Addition Monoid zero") {
    forAll{ x: Int =>
      val actual = intAddition.op(intAddition.zero, x)
      actual should be (x)
    }
  }

  property("Boolean and Monoid op") {
    forAll{(x: Boolean, y: Boolean) =>
      val actual = booleanAnd.op(x, y)
      actual should be (x && y)
    }
  }

  property("Boolean and Monoid zero") {
    forAll{ x: Boolean =>
      val actual = booleanAnd.op(booleanAnd.zero, x)
      actual should be (x)
    }
  }

  property("Boolean or Monoid op") {
    forAll{(x: Boolean, y: Boolean) =>
      val actual = booleanOr.op(x, y)
      actual should be (x || y)
    }
  }

  property("Boolean or Monoid zero") {
    forAll{ x: Boolean =>
      val actual = booleanOr.op(booleanOr.zero, x)
      actual should be (x)
    }
  }

}
