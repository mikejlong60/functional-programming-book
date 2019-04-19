package chapter7.nonblocking

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.scalactic.TypeCheckedTripleEquals._ 
import Nonblocking.Par._
import java.util.concurrent._

class NonBlockingParTest extends PropSpec with PropertyChecks with Matchers {
  val executor = Executors.newFixedThreadPool(5)

  def sumInParallel[A](ints: List[Int])(es: ExecutorService): Nonblocking.Par[Int] = {//Try sequencebalanced
    if (ints.size <= 1) {
      val p = unit(ints.headOption getOrElse 0)
      p
    } else {
      val (l, r) = ints.splitAt(ints.length/2)
      map2(lazyUnit(sumInParallel(l)(es)), lazyUnit(sumInParallel(r)(es)))((x, y) => {
        val xx = Nonblocking.Par.run(es)(x)
        val yy =  Nonblocking.Par.run(es)(y)
        xx.get + yy.get
      }
      )
    }
  }

//  property("prove that you don't have a deadlock as in exercise 7.9") {
//    forAll { xs: List[Int] =>
//      val actual = Nonblocking.Par.run(executor)(sumInParallel(xs)(executor))
//      val expected = xs.sum
//      actual.get should be (expected)
//    }
//  }

  property("run choice") {
    forAll {choice: Boolean =>
      val t = lazyUnit("it was true")
      val f = lazyUnit("it was false")
      val a = Nonblocking.Par.choice(unit(choice))(t, f)
      val actual = Nonblocking.Par.run(executor)(a).get
      if (choice) actual should be ("it was true")
      else actual should be ("it was false")
    }
  }

    property("run choiceUsingChoiceN") {
    forAll {choice: Boolean =>
      val t = lazyUnit("it was true")
      val f = lazyUnit("it was false")
      val a = Nonblocking.Par.choiceUsingChoiceN(unit(choice))(t, f)
      val actual = Nonblocking.Par.run(executor)(a).get
      if (choice) actual should be ("it was true")
      else actual should be ("it was false")
    }
  }

  property("run choiceN") {
    forAll {choice: Boolean =>
      val t = lazyUnit("it was true")
      val f = lazyUnit("it was false")
      val c = if (choice) 0 else 1
      val a = Nonblocking.Par.choiceN(unit(c))(List(t, f))
      val actual = Nonblocking.Par.run(executor)(a).get
      if (choice) actual should be ("it was true")
      else actual should be ("it was false")
    }
  }

  property("prove that parMap does not deadlock") {
    val xs = 1 to 100 toList

    println("piss1")
    val a = Nonblocking.Par.parMap(xs)(math.sqrt(_))
    val actual = Nonblocking.Par.run(executor)(a).get
    println("piss2")
    val expected = xs.map(math.sqrt(_))
    actual should be (expected)
  }

  def lawOfFork[A](a: Nonblocking.Par[A])(es: ExecutorService): Boolean = {
    val aa = Nonblocking.Par.run(es)(fork(a))
    val bb = Nonblocking.Par.run(es)(a)
    aa == bb
  }
  property("law of fork") {
    forAll{(x: Int, xs: List[Int]) =>
      val g  = (i: Int) =>  i + x
      lawOfFork(unit(g))(executor) should be (true)
      lawOfFork(unit(x))(executor) should be (true)
      lawOfFork(unit(xs))(executor) should be (true)
    }
  }
}

