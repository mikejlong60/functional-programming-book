package chapter8

import chapter6.{SimpleRNG, RNG, State}
import org.scalacheck._
import Prop.{forAll, propBoolean}

object GenTest extends Properties("Generator test") {

  property("Run choose within a range") =
    forAll{(start: Short, stopExclusive: Int) =>
      (stopExclusive > start) ==> {
        val actual = chapter8.Gen.choose(start, stopExclusive)
        val rng = SimpleRNG(start)
        val fff =actual.sample.run(rng)
        fff._1  >= start.toInt
        fff._1  <  stopExclusive
      }
    }

  property("Run unit which always generates a value of A") =
    forAll{a: Int =>
      val actual = chapter8.Gen.unit(a)
      val rng = SimpleRNG(System.currentTimeMillis)
      actual.sample.run(rng)._1 == a
    }


  property("Run boolean which always generates a random boolean value") =
    forAll{a: Int =>
      val rng = SimpleRNG(a)
      val actual = chapter8.Gen.boolean.sample.run(rng)._1
      val ex = RNG.nonNegativeInt(rng)
      val expected =  ex._1 % 2 == 0
      actual == expected
    }

  property("Generate a list of length n using the generator g") =
    forAll{(n : Short, x: Int) =>
      (n > 0) ==> {
        val rng = SimpleRNG(x)
        val g = chapter8.Gen.listOfN(n, chapter8.Gen.choose(1, 5))
        val result: List[Int]  = g.sample.run(rng)._1
        result.contains(1) || result.contains(2) || result.contains(3) || result.contains(4) || result.contains(5) && result.size == n
      }
    }

  property("Generate a list of random lists, each inner list having a length of 7 random values between 50 and 700") = {
    val nn = chapter8.Gen.choose(50, 100)
    val rng = SimpleRNG(12)
    val g = chapter8.Gen.listOfN(7, chapter8.Gen.choose(50, 700))
    val gg  = chapter8.Gen.listOfN2(nn, g)
    val result  = gg.sample.run(rng)._1
    result.map(hh => hh.size == 7)
    result.size >= 50 &&  result.size <= 100
  }

  property("Generate the union of two generators") = {
    val first = chapter8.Gen.choose(5,6)
    val second = chapter8.Gen.choose(8, 9)
    val rng = SimpleRNG(12)
    val result = chapter8.Gen.union(first, second).sample.run(rng)
    result._1 == 5 || result._1 == 6 || result._1 == 8 || result._1 == 9
  }

 property("Generate the weighted union of two generators") = {
    val first = chapter8.Gen.choose(5,6)
    val second = chapter8.Gen.choose(8, 9)
    val rng = SimpleRNG(12)
    val result = chapter8.Gen.weighted((first, 100), (second, 1)).sample.run(rng)
   result._1 == 5 || result._1 == 6 || result._1 == 8 || result._1 == 9
  }

  property("Use Map to produce a new generator") =
    forAll{ n: Int =>
      val a = chapter8.Gen.unit(n)
      val r = a.map(s => s.toString)
      val rng = SimpleRNG(n)
      val x = r.sample.run(rng)
      x._1 == n.toString
    }

  property("Use flatMap to produce a new generator") =
    forAll{ n: Int =>
      val a = chapter8.Gen.unit(n)
      val r = a.flatMap(s => chapter8.Gen.unit(s.toString))
      val rng = SimpleRNG(n)
      val x = r.sample.run(rng)
      x._1 == n.toString
    }

  property("mapping a generator using its ID function results in the same generator") =
    forAll{ n: Int =>
      val a = chapter8.Gen.unit(n)
      val b = a.map(chapter8.Gen.id)
      val rng = SimpleRNG(n)
      val actual = b.sample.run(rng)
      val expected = a.sample.run(rng)
      actual == expected
    }
}


