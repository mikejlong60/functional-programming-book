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

  property("Make a parallel version of foldMap") {
    import chapter7.nonblocking.Nonblocking.Par
    val es = java.util.concurrent.Executors.newFixedThreadPool(8)

    forAll {xs: IndexedSeq[Int] =>
      val expected = chapter7.nonblocking.Try(xs.sum * 100)
      val startTs = System.currentTimeMillis
      val actual = Par.run(es)(Monoid.parFoldMap(xs, intAddition)(a => a * 100))
      actual should be (expected)
    }
  }

  property("Int Ordered Monoid associative law") {
    forAll{ (x1: Int, x2: Int, x3: Int) =>
      val xs1 = (true, x1)
      val xs2 = (true, x2)
      val xs3 = (true, x3)
        associativeLawTest(intOrdered)(xs1, xs2, xs3)
    }
  }

  property("Int Ordered Monoid zero law") {
    forAll{ (x: Int, o: Boolean) =>
      zeroLawTest(intOrdered)((o,x))
    }
  }

  property("Write a foldMap to detect if a given sequence is ordered") {
    forAll {xs: IndexedSeq[Int] => //TODO

  }

  }



  property("Show that the parellel version of foldMap produces much better throughput given the limitation on list size. See the comment on sequenceBalanced in chapter7.nonblocking.NonBlocking") {
    import chapter7.nonblocking.Nonblocking.Par
    val es = java.util.concurrent.Executors.newFixedThreadPool(8)

    val xs =(1 to 100)
    val start1 = System.currentTimeMillis
    val actual = Par.run(es)(Monoid.parFoldMap(xs, intAddition)(a => {
      Thread.sleep(1)
      a * 100
    }))
    val actual1 = System.currentTimeMillis() - start1
    println(s"Parallel foldMap took $actual1")

    val expectedSlow = xs.foldLeft(0)((b, a) => (a * 10) + b)
    val start2 = System.currentTimeMillis()

    val actualSlow = Monoid.foldMapV(xs, intAddition)(a => {
      Thread.sleep(1)
      a * 100
    })
    val actual2 = System.currentTimeMillis() - start2
    println(s"syncronous foldMap took $actual2")
    actual1 should be <  (actual2)
  }


}
