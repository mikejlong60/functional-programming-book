package chapter8

import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks
import chapter6.{RNG, SimpleRNG, State}
import chapter8.types._


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

  property("Test your own property, running 120 test cases. Its not scalacheck's property.  Its mine.") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual = Prop.forAll(gen)(x =>  x  > 0  && x < 1001)
    val result = actual.run(120, rng)
    result should be (Passed)
  }

  property("Compose two properties with &&") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen, "First")(x =>  x  > 0  && x < 1001)
    val actual2= Prop.forAll(gen, "Second")(x =>  x  > 0)
    val combinedActual = actual1.&&(actual2)
    val result = combinedActual.run(120, rng)
    result should be (Passed)
  }

  property("Compose two properties with ||") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen, "First")(x =>  x  < 0)
    val actual2= Prop.forAll(gen, "Second")(x =>  x  > 0)
    val combinedActual = actual1.||(actual2)
    val result = combinedActual.run(120, rng)
    result should be (Passed)
  }

  property("Compose failure of two properties with &&") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen, "First")(x =>  x  > 0  && x < 1001)
    val actual2= Prop.forAll(gen, "Second")(x =>  x  < 0)
    val combinedActual = actual1.&&(actual2)
    val result = combinedActual.run(120, rng)
    result shouldBe a  [Falsified]
  }

  property("Compose failure of  two properties with ||") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen, "First")(x =>  x  < 0)
    val actual2= Prop.forAll(gen, "Second")(x =>  x  < 0)
    val combinedActual = actual1.||(actual2)
    val result = combinedActual.run(120, rng)
    result  shouldBe a [Falsified]
  }

  property("Compose three properties with &&, have one fail,  and tell the tester which property caused the failure.") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen, "gt zero and lt 1001")(x =>  x  > 0  && x < 1001)
    val actual2= Prop.forAll(gen, "gt zero")(x =>  x  > 0)
    val actual3= Prop.forAll(gen, "less than zero")(x =>  x  < 0)
    val combinedActual = actual1.&&(actual2).&&(actual3)
    val result = combinedActual.run(120, rng)
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
    val result = combinedActual.run(120, rng)
    result match {
      case Falsified(propName,  _ , _) => propName should be ("gt 4000")
      case _ => fail(s"result was [$result] but  should have been falsified")
    }
  }

}
