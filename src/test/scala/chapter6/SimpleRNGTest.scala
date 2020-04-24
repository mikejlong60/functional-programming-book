package chapter6

import RNG._
import org.scalacheck._
import Prop.{forAll, propBoolean}

object SimpleRNGTest extends Properties("RNG tests") {

  property("Generating two random numbers using the same generator produces the same number ") =
    forAll { x: Int =>
      val rng = SimpleRNG(x)
      val (n1, rng2) = rng.nextInt
      val (n2, rng3) = rng.nextInt
      n1 ==n2
    }

  property("Generate a non-negative random number") =
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = nonNegativeInt(rng: RNG)
      actual._1 >= 0
    }

  property("Generate a non-negative double between 0 and 1") =
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = double(rng: RNG)
      actual._2 != rng
      actual._1.toInt ==  0
    }

  property("Generate pairs of random numbers") =
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = intDouble(rng)
      actual._2 != rng
      actual._1._1 >= 0
      actual._1._2.toInt == 0
    }

  property("Generate pairs of random numbers in opposite order") =
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = doubleInt(rng)
      actual._2 != rng
      actual._1._2 >= 0
      actual._1._1.toInt == 0
    }

  property("Generate triple of random numbers") =
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = double3(rng)
      actual._2 != rng
      actual._1._1.toInt == 0
      actual._1._2.toInt == 0
      actual._1._3.toInt == 0
    }

  property("Generate list of random Ints") =
    forAll {x: Short =>
      (x > 0 && x < 4000) ==> {
        val rng = SimpleRNG(x)
        val actual = ints(x)(rng)//Because this is not tail recursive it blows up.  The book solution shows a tail-recursive version with an inner function.
        actual._1.size == x
      }
    }

  property("Generate a non-negative double between 0 and 1 with map") =
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val actual = doubleMap(rng)
      actual._2 != rng
      actual._1.toInt == 0
    }
 
  property("Generate an Int and  Double pair using map2") =
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val fff = map2(nonNegativeInt, doubleMap)((_,_))
      val actual = fff(rng)
      actual._2 != rng
      actual._1._1 >= 0
      actual._1._2.toInt == 0
    }

  property("Generate a Double  Double pair using map2") =
    forAll {x: Int =>
      val rng = SimpleRNG(x)
      val fff = map2(doubleMap, doubleMap)((_, _))
      val actual = fff(rng)
      actual._2 != rng
      actual._1._1.toInt == 0
      actual._1._2.toInt == 0
    }

  property("Generate a sequence of random doubles") =
    forAll{(xs: List[Int], x: Int) =>
      val rng = SimpleRNG(x)
      val xxs = xs.map(x => doubleMap)
      val fff = sequence(xxs)
      val actual = fff(rng)
      actual._1.size == (xs.size)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def nonNegativeLessThanFm(n: Int): Rand[Int] = {
     flatMap(nonNegativeInt)(i => {
       val mod = i % n
       if (i  +  (n - 1) - mod >= 0) unit(mod) //rng =>  (mod, rng)
       else  nonNegativeLessThanFm(n)
    })
  }

  property("Use flatMap for nonNegativeLessThan") =
    forAll{x: Int =>
      (x > 0) ==> {
      val rng = SimpleRNG(x)
      val result = nonNegativeLessThanFm(x)
      val actual = result(rng)
      actual._1 >= 0
      actual._1  < x
      }
    }

  property("Use flatMap") =
    forAll{(xs: List[Int], x: Int) =>
     val rng = SimpleRNG(x)
     val xxs = xs.map(x => {
       val r = unit(x)
       val res = flatMap(r)(d => unit(x))
       res
     })
     val fff = sequence(xxs)
     val actual = fff(rng)
     actual._1.size == (xs.size)
    }

  property("Reveal bug in roll die. This should fail") = {
    def rollDie: Rand[Int] = nonNegativeLessThan(6)

    forAll { x: Int =>
      val actual = rollDie(SimpleRNG(x))
      actual._1 > 0
      actual._1 <= 6
    }
  }

  property("Fix bug in roll die") = {
    def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(x => x + 1)

    forAll { x: Int =>
      val actual = rollDie(SimpleRNG(x))
      actual._1 > 0
      actual._1 <= 6
    }
  }
}
