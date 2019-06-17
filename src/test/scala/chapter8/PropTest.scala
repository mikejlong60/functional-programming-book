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
    val result = actual.run(0, 120, rng)
    result should be (Passed(0))
  }

  property("Compose two properties with and") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen)(x =>  x  > 0  && x < 1001)
    val actual2= Prop.forAll(gen)(x =>  x  > 0)
    val combinedActual = actual1.&&(actual2)
    val result = combinedActual.run(0, 120, rng)
    result should be (Passed(0))
  }

  property("Compose two properties with or") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen)(x =>  x  < 0)
    val actual2= Prop.forAll(gen)(x =>  x  > 0)
    val combinedActual = actual1.||(actual2)
    val result = combinedActual.run(0, 120, rng)
    result should be (Passed(0))
  }

  //TODO figure out how to return some value that tells the author of the test which property caused the failure.  This
  //applied to both `and` and `or`.  For both these cases either  the first or second caused the failure.  

  property("Compose failure of two properties with and") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen)(x =>  x  > 0  && x < 1001)
    val actual2= Prop.forAll(gen)(x =>  x  < 0)
    val combinedActual = actual1.&&(actual2)
    val result = combinedActual.run(0, 120, rng)
    result shouldBe a  [Falsified]
  }

  property("Compose failure of  two properties with or") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen)(x =>  x  < 0)
    val actual2= Prop.forAll(gen)(x =>  x  < 0)
    val combinedActual = actual1.||(actual2)
    val result = combinedActual.run(0, 120, rng)
    result  shouldBe a [Falsified]
  }

  property("Compose failure of  two properties with and and tell the tester which property caused the failure.") {
    val gen = Gen.choose(1, 1000)
    val rng = SimpleRNG(1)
    val actual1= Prop.forAll(gen)(x =>  x  > 0  && x < 1001)
    val actual2= Prop.forAll(gen)(x =>  x  > 0)
    val actual3= Prop.forAll(gen)(x =>  x  < 0)
    val combinedActual = actual1.&&(actual2).&&(actual3)
    val result = combinedActual.run(0, 120, rng)
    result shouldBe a [Falsified]
    result.fstFailure shouldBe (2)
  }

}
