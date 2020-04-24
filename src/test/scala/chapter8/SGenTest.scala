package chapter8

import chapter6.{RNG, SimpleRNG, State}
import org.scalacheck._
import Prop.{forAll, propBoolean}

object SGenTest extends Properties("Sized generator test") {


  property("Use SGen Map to produce a new SGen from unit") =
    forAll{ n: Int =>
      val a = chapter8.Gen.unit(n)
      val b = a.unsized
      val c = b.map(x => x * 1000)
      val d = c(n)
      val rng = SimpleRNG(n)
      val e = d.sample.run(rng)
      e._1 == (n * 1000)
    }

  property("Use flatMap to produce a new SGen from unit") =
    forAll{ n: Int =>
      val a = chapter8.Gen.unit(n)
      val b = a.unsized
      val intToString: (Int => chapter8.Gen[String]) = a => chapter8.Gen.unit(a.toString)
      val c = b.flatMap(x => intToString(x).unsized)
      val d = c(n)
      val rng = SimpleRNG(n)
      val e = d.sample.run(rng)
      e._1 == n.toString
    }

  property("Generate a list of length n using the generator g") =
    forAll{(n : Short) =>
      (n > 0) ==> {
        val rng = SimpleRNG(System.currentTimeMillis())
        val a = chapter8.Gen.choose(19, 21)
        val b = a.unsized
        val c = b.listOf(a)
        val d = c(n)
        val result = d.sample.run(rng)
        result._1.contains(19) || result._1.contains(20) || result._1.contains(21)  && result._1.size == n
      }
    }
}


