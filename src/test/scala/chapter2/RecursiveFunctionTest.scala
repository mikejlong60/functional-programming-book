package chapter2

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class RecursiveFunctionTest extends PropSpec with PropertyChecks with Matchers {

  property("Test tail-recursive Fibonacci function") {


    def fib(n: Long): Long = {

      @annotation.tailrec
      def internal(fst: Long, snd: Long, counter: Long): Long =
        if (counter == n) fst + snd
        else internal(snd, fst + snd, counter + 1)

      if (n <= 1) n
      else internal(0, 1, 2)
    }


    forAll { n: Short =>
      whenever(n >= 0) {
        println(s"COMPUTING fib($n)")
        println(s" fib($n) ${fib(n)}")
        println(s" fib(2) ${fib(2)}")
        fib(0) should be(0)
        fib(1) should be(1)
        fib(2) should be(1)
        fib(3) should be(2)
        fib(4) should be(3)
        fib(5) should be(5)
        fib(6) should be(8)
        fib(7) should be(13)
        fib(8) should be(21)
        fib(9) should be(34)
        fib(10) should be (55)
        fib(11) should be (89)
        fib(12) should be (144)
        fib(13) should be (233)
        fib(14) should be (377)
        fib(15) should be (610)
        fib(16) should be (987)
        fib(17) should be (1597)
        fib(18) should be (2584)
        fib(57) should be (365435296162L)
      }
    }
  }
}


