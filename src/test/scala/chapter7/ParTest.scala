package chapter7

import chapter7.Par._
import java.util.concurrent._
import org.scalacheck._
import Prop.{forAll, propBoolean}

object ParTest extends Properties("Blocking Par test") {

  val executor = Executors.newFixedThreadPool(8)

  property("sum in parallel NOT ") =
    forAll { xs: List[Int] => {
      val actual = sumInParallelNot(xs)
      val expected = xs.sum
      actual == expected
    }
   }

  property("force a deadlock as in exercise 7.9") = {
    val executor = Executors.newFixedThreadPool(2)//Making this size of the list bigger than the number of threads in the thread pool causes the current implementation of fork to block because it splits the problem into more pieces than I have threads.  I will fix that later in the chapter.
    val xs  = List(1,2,3,4)
    try {
      sumInParallel(xs)(executor).get
      executor.shutdown()
      false
    } catch {
      case e => {
        executor.shutdown()
        true
      }
    }
  }

  property("don't force a deadlock as in exercise 7.9") = {// Thread pool is big enough
    val xs = List(1,2,3,4)
    val executor = Executors.newFixedThreadPool(10)
    val actual = sumInParallel(xs)(executor)
    val expected = xs.sum
    executor.shutdown()
    actual.get == expected
  }

  property("use lazyUnit") = {
    val actual = Par.lazyUnit(12)(executor).get
    val ggg =Par.unit(12)(executor).get
    actual == ggg
  }

  property("use asyncF") = {
    val f = (a: Int) => a + 12
    f(36) == 48
    val a1 = Par.asyncF(f)
    val actual =   a1(36)(executor).get
    actual == 48
  }

  property("use other asyncF") = {
    val f = (a: Int) => a + 12
    f(36) == 48
    val a1 = Par.asyncF2(f)
    val actual =   a1(36)(executor).get
    actual == 48
  }

  property("parallel sorting") =
    forAll{ xs: List[Int] => 
      val actual = Par.sortPar(unit(xs))(executor).get
      actual == (xs.sorted)
    }

  property("use map") =
    forAll{ xs: List[Int] =>
      val actual = Par.map(unit(xs))(a => a.map(aa => aa + 12))(executor).get
      actual == (xs.map(aa => aa + 12))
    }

  property("use parMap") =
    forAll{ xs: List[Int] =>
      val actual = Par.parMap(xs)(a => a + 12)(executor).get
      actual == (xs.map(aa => aa + 12))
    }

  property("use sequence to sum a big list in parallel") =
    //Better sum in parallel
    forAll{(x1: List[Int], x2: List[Int], x3: List[Int]) => 
      val actual = Par.sequence(List(
        unit(x1.sum),
        unit(x2.sum),
        unit(x3.sum)
      ))(executor).get.sum
      actual == (x1.sum + x2.sum + x3.sum)
    }

  property("use sequence to filter a list in parallel") =
    //Better filter in parallel
    forAll{xs: List[Int] => 
      val (l, r) = xs.splitAt(xs.length/2)
       val actual = Par.sequence(List(
        unit(l.filter(x => x > 0)),
        unit(r.filter(x => x > 0))
      ))(executor).get.flatten.sorted

       val xx = xs.filter(x => x > 0).sorted
      actual == xx
    }

  property("use filter  in parallel") =
    forAll{xs: List[Int] =>
      val actual = Par.parFilter(xs)(x => x >0)(executor).get.sorted
      val xx = xs.filter(x => x > 0).sorted
      actual == xx
    }

  property("another law of mapping") =
    forAll{(x: Int, xs: List[Int]) =>
      val g  = (i: Int) =>  i + x
      lawOfMap(unit(g))(executor) == true
      lawOfMap(unit(x))(executor) == true
      lawOfMap(unit(xs))(executor) == true
    }

  property("law of fork") =
    forAll{(x: Int, xs: List[Int]) =>
      val g  = (i: Int) =>  i + x
      lawOfFork(unit(g))(executor) == true
      lawOfFork(unit(x))(executor) == true
      lawOfFork(unit(xs))(executor) == true
    }

  property("run delay") =
    forAll{(x: Int, xs: List[Int]) =>
      val g = (i: Int) => i  + x
      val actual = delay(unit(g))(executor).get()(x)
      true
    }
}

