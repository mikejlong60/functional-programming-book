package chapter8

import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks
import chapter6.{RNG, SimpleRNG, State}
import chapter8.types._
import scala.collection.immutable.List._
import chapter7.Par
import chapter7.Par.lawOfMap
import Gen.{parIntGen, parListOfN}
import java.util.concurrent._

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

  property("Test mapping law where Par.map(Par.unit(1))(x => x + 1) == Par.unit(2)" ) {
    import java.util.concurrent.Executors
    val executor = Executors.newFixedThreadPool(8)
    val result = Par.equal(
      Par.map(Par.unit(1))(x => x + 1),
      Par.unit(2)
    )
    val r = Par.map(result)(r => r)(executor).get
    r should be (true)
  }

  property("Run forAllPar to prove the law of map(mapping the identity function over a computation should have no effect) using parIntGen") {
    val rng = SimpleRNG(System.currentTimeMillis())
    val p = Prop.forAllPar(parIntGen(1, 1000))(n => Par.equal(Par.map(n)(y => y), n))

    val result1 = p.run(100, 100,  rng)
    result1 should be (Passed)
  }

  property("Run forAllPar to prove the law of map(mapping the identity function over a computation should have no effect) using  parListOfN") {
    val nn: Gen[Int] = Gen.choose(50, 100)
    val rng = SimpleRNG(12)
    val g: Gen[List[Int]] = Gen.listOfN(7, Gen.choose(50, 700))
    val gg  = parListOfN(nn, g)
    val p = Prop.forAllPar(gg)(n => Par.equal(Par.map(n)(y => y), n))

    val result1 = p.run(100, 100,  rng)
    result1 should be (Passed)
  }

  property("Prove the law of map(mapping the identity function over a computation should have no effect) and law of fork(forking a computation across threads should not produce a different result than running it in a single thread )using  parListOfN") {
    val nn: Gen[Int] = Gen.choose(50, 100)
    val rng = SimpleRNG(12)
    val g: Gen[List[Int]] = Gen.listOfN(7, Gen.choose(50, 700))
    val gg  = parListOfN(nn, g)

    val r1 = Par.lawOfMap(Par.unit(gg))(Executors.newFixedThreadPool(8))
    r1 should be (true)
    val r2 =  Par.lawOfFork(Par.unit(gg))(Executors.newFixedThreadPool(8))
    r2 should be (true)
  }

  property("Run Prop.forAll with a generated function to prove the relationship between takewhile and dropwhile") {
    forAll  { (xs: List[Int]) =>
    val numberOfTestCases = 100
    val maxSizeOfGenerator = 12
    val rng = SimpleRNG(System.currentTimeMillis())
    val a = Gen.choose(19, 23)
    val b = a.unsized
    val c = b.listOf(a)
    val dF = Gen.dtakeWhileDropWhileF(a)(List(19,20,21))//xs)
    //val e: Prop = Prop.forAll(c, "list members must be either 19, 20,  21, or 22 ")(l => {
      val r = dF.map(p => xs.forall(p))
      val gg = Prop.forAll(r)(h => h)
      val result = gg.run(maxSizeOfGenerator, numberOfTestCases, rng)
      //l.forall(m => m == 19 || m ==20 || m== 21 || m == 22)
   // })
   //val result = e.run(maxSizeOfGenerator ,numberOfTestCases, rng)
       result should be (Passed)
     }
  }


}
