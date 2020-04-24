package chapter2

import org.scalacheck._
import Prop.{forAll, propBoolean}

class RecursiveFunctionTest extends Properties("Recursive Functions") {

  def fib(n: Long): Long = {

    @annotation.tailrec
    def internal(fst: Long, snd: Long, counter: Long): Long =
      if (counter == n) fst + snd
      else internal(snd, fst + snd, counter + 1)

    if (n <= 1) n
    else internal(0, 1, 2)
  }

  property("Test tail-recursive Fibonacci function") =
    forAll { n: Short =>
      (n >= 0) ==> {
        fib(0)  == (0)
        fib(1) == (1)
        fib(2) == (1)
        fib(3) == (2)
        fib(4) == (3)
        fib(5) == (5)
        fib(6)  == (8)
        fib(7) == (13)
        fib(8) == (21)
        fib(9) == (34)
        fib(10) == (55)
        fib(11) == (89)
        fib(12) == (144)
        fib(13) == (233)
        fib(14) == (377)
        fib(15) == (610)
        fib(16) == (987)
        fib(17) == (1597)
        fib(18) == (2584)
        fib(57) == (365435296162L)
      }
    }

}


