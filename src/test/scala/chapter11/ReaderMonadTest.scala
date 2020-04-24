package chapter11

import scala.{Option => _, None => _, Some => _,  Right => _, Left => _, Either => _, _}
import org.scalacheck._
import Prop.{forAll, propBoolean}

object  ReaderMonadTest extends Properties("Reader Monad test") {

  val mon = chapter11.Monad.readerMonad[Int]
  val f1 = Reader((x:Int)  => x.toLong)
  val f2 = (x: Long) => x * 1000
  val f3 = Reader(f2)

  property("Test map function") =
    forAll { x: Short =>
      val actual = mon.map(f1)(f2).run(x)
      val expected = (x * 1000).toLong
      actual == expected
    }

  property("Test map2 function") =
    forAll { (x: Short) =>
      val actual = mon.map2(f1, f1)((x, y) => x + y).run(x)
      val expected = x * 2
      actual == expected
    }

  property("Test map3 function ") =
    forAll { (x: Short) =>
      val r1 = Reader((x:Int)  => x.toLong  - 3)
      val r2 = Reader((x:Int)  => x.toLong + 3)
      val r3 = Reader((x:Int)  => x.toLong + 30)

      val actual = mon.map3(r1,r2,r3)((x, y, z) => x + y + z).run(x)
      val expected = (x - 3) + (x + 3) + (x + 30)
      actual == expected
    }

  property("Test Map Law") =
    forAll{x: Int =>
      val actual = mon.map(f1)(a => a).run(x)
      actual == x
    }

  property("Test Associative Law") =
    forAll { (x: Int) =>
      val m = Reader((x:Int)  => x  - 3)
      val f = (x:Int)  => mon.unit(x + 3)
      val g = (x:Int)  => mon.unit(x + 30)

      val left = mon.flatMap(mon.flatMap(m)(f))(g).run(x)
      val right = mon.flatMap(m)(a => mon.flatMap(f(a))(g)).run(x)
      left == right
    }

  property("Test Kleisli Associative Law") =
    forAll {x: Int  =>
     val m = Reader((x:Int)  => x  - 3)
      val f = (x:Int)  => mon.unit(x + 3)
      val g = (x:Int)  => mon.unit(x + 30)
      val h = (x:Int)  => mon.unit(x + 300)

      val lf = mon.compose(mon.compose(f, g),h)
      val rf = mon.compose(f,(mon.compose(g,h)))
      val left = lf(x).run(x)
      val right = rf(x).run(x)
      left == right
    }

  property("Prove identity laws using Kleisli composition") =
    forAll {x: Int  =>
      val f = (x:Int)  => mon.unit(x)
      val li = mon.compose(f, (b: Int) => mon.unit(b))
      val ri = mon.compose((a: Int) => mon.unit(a), f)
      val left = li(x)
      val right = ri(x)
      left.run(x) == right.run(x)
    }

  val specialMon = chapter11.Monad.readerMonad[List[String]]
  val f: String => Reader[List[String], Int] = (xs: String) =>
  try {
    val x = xs.toInt
    specialMon.unit(x)
  } catch {
    case e: Exception => specialMon.unit(-1)
  }
  
  property("Test traverse over unparseable number") = {
    val xs = List("11","13a")
    val expected = List(11,-1)
    val r = specialMon.traverse(xs)(f)
    val actual = r.run(xs)
    actual == expected
  }

  property("Test traverse over empty list") = {
    val xs = Nil
    val expected = Nil
    val actual = specialMon.traverse(xs)(f).run(xs)
    actual == expected
  }

  property("Test traverse over list of one element") = {
    val xs = List("12")
    val expected = List(12)
    val actual = specialMon.traverse(xs)(f).run(xs)
    actual == expected
  }

  property("Test replicateM") = {
    val mon = chapter11.Monad.readerMonad[Int ]
    forAll { x: Int =>
      val f = (x:Int)  => x  - 3
      val lll = mon.replicateM(12, Reader(f))
      val actual = lll.run(x)
      val expected = List.fill(12)(x).map(f)
      actual == expected
    }
  }


/**
replicateM allows you to apply the same function n times to the same argument.  So you can 
make replicateM apply the function only once instead of calling it many times by overrideing it 
in the Reader Monad.  You have not done that yet but you could.

The sequence function takes a list of functions and takes the one argument and passes it
to every function in the list and returns the resulting list.

  */

  property("Test sequence for list of functions") = {
    val mon = chapter11.Monad.readerMonad[Int ]
    forAll { (x: Int) =>
      val x = 13
      val y = 100
      val f = (x:Int)  => x  - 3
      val xs = List.fill(y)(Reader(f))
      val actual = mon.sequence(xs).run(x)
      actual == List.fill(y)(x -3)
    }
  }

  property("Test sequence for empty list of functions") = {
    val mon = chapter11.Monad.readerMonad[Int]
    forAll { (x: Int) =>
      val f = (x: Int) => x - 3
      val r: Reader[Int, Int] = Reader(f)
      val xs = List.empty[Reader[Int, Int]]
      val actual = mon.sequence(xs).run(x)
      actual == List()
    }
  }
}

