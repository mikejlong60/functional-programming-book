package chapter8

import chapter6.{SimpleRNG, RNG, State}
import chapter6.{RNG, SimpleRNG, State}
import chapter8.types._
import scala.collection.immutable.List._
import chapter7.Par
import chapter7.Par.lawOfMap
import Gen.{parIntGen, parListOfN}
import java.util.concurrent._
import org.scalacheck._
import Prop.{forAll, propBoolean}

object SGenTest extends Properties("Sized generator test") {


//  property("Use SGen Map to produce a new SGen from unit") {
//    forAll{ n: Int =>
//      val a = Gen.unit(n)
//      val b = a.unsized
//      val c = b.map(x => x * 1000)
//      val d = c(n)
//      val rng = SimpleRNG(n)
//      val e = d.sample.run(rng)
//      e._1 should be (n * 1000)
//    }
//  }
//
//  property("Use flatMap to produce a new SGen from unit") {
//    forAll{ n: Int =>
//      val a = Gen.unit(n)
//      val b = a.unsized
//      val intToString: (Int => Gen[String]) = a => Gen.unit(a.toString)
//      val c = b.flatMap(x => intToString(x).unsized)
//      val d = c(n)
//      val rng = SimpleRNG(n)
//      val e = d.sample.run(rng)
//      e._1 should be (n.toString)
//    }
//  }
//
//  property("Generate a list of length n using the generator g") {
//    forAll{(n : Short) =>
//      whenever(n > 0) {
//        val rng = SimpleRNG(System.currentTimeMillis())
//        val a = Gen.choose(19, 21)
//        val b = a.unsized
//        val c = b.listOf(a)
//        val d = c(n)
//        val result = d.sample.run(rng)
//        result._1 should (contain  (19) or contain(20) or contain(21)  and have size (n))
//      }
//    }
//  }
}


