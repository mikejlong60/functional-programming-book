package chapter8

import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks
import chapter6.{RNG, SimpleRNG, State}
import chapter8.types._
import scala.collection.immutable.List._


class PropTest extends PropSpec with PropertyChecks with Matchers {

  property("Check first property") {
    forAll{(check1: Boolean, check2: Boolean) =>
//      val p = new Prop{
//        def check = check1
//      }
//      val p2 = new Prop {
//        def check = check2
//      //}

 //     val expected = new Prop {
 //       def check = check1 && check2
 //     }
 //     p2.&&(p).check should be (expected.check)
    }
  }

  val maxSize = 12
  property("Test your own property, running 120 test cases. Its not scalacheck's property.  Its mine.") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual = Prop.forAll(gen)(x =>  x  > 0  && x < 1001)
    val result = actual.run(maxSize, 120, rng)
    result should be (Passed)
  }

  property("Compose two properties with &&") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen, "First")(x =>  x  > 0  && x < 1001)
    val actual2= Prop.forAll(gen, "Second")(x =>  x  > 0)
    val combinedActual = actual1.&&(actual2)
    val result = combinedActual.run(maxSize, 120, rng)
    result should be (Passed)
  }

  property("Compose two properties with ||") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen, "First")(x =>  x  < 0)
    val actual2= Prop.forAll(gen, "Second")(x =>  x  > 0)
    val combinedActual = actual1.||(actual2)
    val result = combinedActual.run(maxSize, 120, rng)
    result should be (Passed)
  }

  property("Compose failure of two properties with &&") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen, "First")(x =>  x  > 0  && x < 1001)
    val actual2= Prop.forAll(gen, "Second")(x =>  x  < 0)
    val combinedActual = actual1.&&(actual2)
    val result = combinedActual.run(maxSize, 120, rng)
    result shouldBe a  [Falsified]
  }

  property("Compose failure of  two properties with ||") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen, "First")(x =>  x  < 0)
    val actual2= Prop.forAll(gen, "Second")(x =>  x  < 0)
    val combinedActual = actual1.||(actual2)
    val result = combinedActual.run(maxSize, 120, rng)
    result  shouldBe a [Falsified]
  }

  property("Compose three properties with &&, have one fail,  and tell the tester which property caused the failure.") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen, "gt zero and lt 1001")(x =>  x  > 0  && x < 1001)
    val actual2= Prop.forAll(gen, "gt zero")(x =>  x  > 0)
    val actual3= Prop.forAll(gen, "less than zero")(x =>  x  < 0)
    val combinedActual = actual1.&&(actual2).&&(actual3)
    val result = combinedActual.run(maxSize, 120, rng)
    result match {
      case Falsified(propName,  _ , _) => propName should be ("less than zero")
      case _ => fail("should have been falsified")
    }
  }

  property("Compose three properties with ||, have all fail,  and tell the tester which property caused the failure.  || will keep trying until the last one and if that fails it gets blamed.") {
    val gen = Gen.choose(2000, 3000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen, "gt zero and lt 1001")(x =>  x  > 0  && x < 1001)
    val actual2= Prop.forAll(gen, "less than zero")(x =>  x  < 0)
    val actual3= Prop.forAll(gen, "gt 4000")(x =>  x  > 4000)
    val combinedActual = actual1.||(actual2).||(actual3)
    val result = combinedActual.run(maxSize, 120, rng)
    result match {
      case Falsified(propName,  _ , _) => propName should be ("gt 4000")
      case _ => fail(s"result was [$result] but  should have been falsified")
    }
  }

  property("Run Prop.forAll with an SGen[A] instead of a Gen[A].  You have come close to making ScalaCheck!!!!") {
    val numberOfTestCases = 100
    val maxSizeOfGenerator = 12
    val rng = SimpleRNG(System.currentTimeMillis())
    val a = Gen.choose(19, 23)
    val b = a.unsized
    val c = b.listOf(a)
    val e = Prop.forAll(c, "list members must be either 19, 20,  21, or 22 ")(l => {
      println(l)
      l.forall(m => m == 19 || m ==20 || m== 21 || m == 22)
    })
   val result = e.run(maxSizeOfGenerator ,numberOfTestCases, rng)
   result should be (Passed)
  }

  property("Run a failing Prop.forAll with an SGen[A] instead of a Gen[A] because the list was too large") {
    val numberOfTestCases = 50
    val maxSizeOfGenerator = 12
    val rng = SimpleRNG(System.currentTimeMillis())
    val a = Gen.choose(19, 23)
    val b = a.unsized
    val c = b.listOf(a)
    val e = Prop.forAll(c, "list members must be either 19, 20,  21, or 22  and the list size must be < 10")(l => {
      l.forall(m => m == 19 || m ==20 || m== 21 || m == 22) && l.size < 10 && l.size < 10
    })
   val result = e.run(maxSizeOfGenerator ,numberOfTestCases, rng)
    result match {
      case Falsified(propName,  _ , _) => propName should be ("list members must be either 19, 20,  21, or 22  and the list size must be < 10")
      case _ => fail(s"result was [$result] but  should have been falsified")
    }
  }

  property("Reveal a bug in your new SGen version of Prop.forAll. Max of an empty list throws an exception") {
    val rng = SimpleRNG(System.currentTimeMillis())
    val a = Gen.choose(-10, 10)
    val b = a.unsized
    val c = b.listOf(a)
    val maxProp = Prop.forAll(c, "value must not exceed max allowed value in the list") {ns =>
      val max = ns.max
      ns.forall(_ <= max)
     } 
   val result = maxProp.run(100 , 100, rng)
    result match {
      case Falsified(propName,  _ , _) => propName should be ("value must not exceed max allowed value in the list")
      case _ => fail(s"result was [$result] but  should have been falsified")
    }
    //This illustrates what happens on a failing test in the UI, just for illustrative purposes as Prop.run returns Unit.
    Prop.run(maxProp)
  }

    property("Fix the bug you showed previously.") {
    val a = Gen.choose(-10, 10)
    val b = a.unsized
    val c = b.nonEmptyListOf(a)
    val maxProp = Prop.forAll(c, "value must not exceed max allowed value in the list") {ns =>
      val max = ns.max
      ns.forall(_ <= max)
     } 
    Prop.run(maxProp)
  }


}
