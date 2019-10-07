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

  property("contatenate test") {
    forAll{ l: IndexedSeq[String] =>
      val actual = Monoid.concatenate(l)(stringMonoid)
      val expected = l.foldLeft("")(_ + _)
      actual should be (expected)
    }
  }

  val blubber = (x: Int) =>  s"We got ${(x  * .65)} pounds of whale oil from this whale which weighed in at $x pounds. \n"

  property("Make foldMap for type that does not have a monoid instance") {
    forAll{whales: IndexedSeq[Int] =>
      val expected = whales.foldLeft("")((b, a) => b +   blubber(a))
      val actual = Monoid.lFoldMap(whales)(stringMonoid)(blubber)
      actual should be (expected)
   }
  }

  property("Write foldLeft using foldMap") {
    forAll{whales: IndexedSeq[Int] =>
      val expected = whales.foldLeft("")((b, a) => b +   blubber(a))
      val actual = Monoid.foldLeft(whales)(stringMonoid)(blubber)
      actual should be (expected) 
    }
  }

    property("Write foldRight using rFoldMap") {
    forAll{whales: IndexedSeq[Int] =>
      val expected = whales.foldRight("")((a, b) => b +   blubber(a))
      val actual = Monoid.foldRight(whales)(stringMonoid)(blubber)
      actual should be (expected) 
    }
    }

  property("Write splitting foldMap") {
    forAll{ xs: IndexedSeq[Int] =>

      val expected = xs.foldLeft(0)((b, a) => (a * 10) + b)

      val actual = Monoid.foldMapV(xs, intAddition)(a => a * 10)
      actual should be (expected)
    }
  }
}
