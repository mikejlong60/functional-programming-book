package chapter10

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import MonoidInstances._

class MonoidInstancesTest extends PropSpec with PropertyChecks with Matchers {

  def zeroLawTest[A](m: Monoid[A])(x: A): org.scalatest.Assertion = m.zeroLaw(x) should be (true)
  def associativeLawTest[A](m: Monoid[A])(x: A, y: A, z: A): org.scalatest.Assertion = m.associativeLaw(x, y, z) should be (true)

  property("String Monoid associative law") {
    forAll{(x: String, y: String, z: String) =>
      associativeLawTest(stringMonoid)(x, y, z)
    }
  }

  property("String Monoid zero law") {
    forAll{ x: String =>
      zeroLawTest(stringMonoid)(x)
    }
  }

  property("Int Addition Monoid associative law") {
    forAll{(x: Int, y: Int, z: Int) =>
      associativeLawTest(intAddition)(x, y, z)
    }
  }

  property("Int Addition Monoid zero law") {
    forAll{ x: Int =>
      zeroLawTest(intAddition)(x)
    }
  }

  property("Boolean and Monoid associative law") {
    forAll{(x: Boolean, y: Boolean, z: Boolean) =>
      associativeLawTest(booleanAnd)(x, y, z)
    }
  }

  property("Boolean and Monoid zero law") {
    forAll{ x: Boolean =>
      zeroLawTest(booleanAnd)(x)
    }
  }

  property("Boolean or Monoid associative law") {
    forAll{(x: Boolean, y: Boolean, z: Boolean) =>
      associativeLawTest(booleanOr)(x, y, z)
    }
  }

  property("Boolean or Monoid zero law") {
    forAll{ x: Boolean =>
      zeroLawTest(booleanOr)(x)
    }
  }

  property("Endofunctor Monoid associative law") {
    forAll{(x: Int) =>
      val f1 = (x: Int) => x + x
      val f2 = (x: Int) => x + 12
      val f3 = (x: Int) => x + 50
      val op1 = endoMonoid.op(endoMonoid.op(f1, f2) , f3)
      val op2 = endoMonoid.op(f1, endoMonoid.op(f2, f3))
      op1(x) should be (op2(x))
    }
  }

  property("Endofunctor Monoid zero law") {
    forAll{ x: Boolean =>
      val f1 = (x: Boolean) => x != true
      val z = endoMonoid.op(f1, endoMonoid.zero)
      z(x) should be (!x)
    }
  }

  property("Option Monoid associative law") {
    forAll{(x: Option[Boolean], y: Option[Boolean], z: Option[Boolean]) =>
      associativeLawTest[Option[Boolean]](optionMonoid)(x, y, z)
    }
  }

  property("Option Monoid zero law") {
    forAll{ x: Option[Boolean] =>
      zeroLawTest[Option[Boolean]](optionMonoid)(x)
    }
  }

}
