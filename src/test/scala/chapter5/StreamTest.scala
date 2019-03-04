package chapter5

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class StreamTest extends PropSpec with PropertyChecks with Matchers {

  property("Test drop function for Stream of ints") {
    forAll { xs: Seq[Int] =>
      val a = Stream.apply(xs:_*)

      val actual = a.drop(1).toList
      val expected = xs.drop(1).toList
      actual should be (expected)
    }
  }

  property("Test take with 1 function for Stream of ints") {
    forAll { xs: Seq[Int] =>
      val a = Stream.apply(xs:_*)

      val actual = a.take(1).toList
      val expected = xs.take(1).toList
      actual should be (expected)
    }
  }

    property("Test take with 3 function for Stream of ints") {
    forAll { xs: Seq[Int] =>
      val a = Stream.apply(xs:_*)
      val actual = a.take(3).toList
      val expected = xs.take(3).toList
      actual should be (expected)
    }
  }

  val p: Int => Boolean = x => x % 2 == 1

  property("Test takewhile for Stream of ints") {
    forAll { xs: Seq[Int] =>
     val actual = Stream.apply(xs:_*).takeWhile(p)
     actual.toList should be (xs.takeWhile(p))
    }
  }

  property("Test toList function for Stream of Ints") {
    forAll { xs: Seq[Int] =>
      val a = Stream.apply(xs:_*)
      val actual = a.toList
       actual should be (xs)
    }
  }

  property("Test exists function for Stream of Ints") {
    forAll { xs: Seq[Int] =>
      val actual  = Stream.apply(xs:_*)
       actual.exists(p) should be (xs.exists(p))
    }
  }

    property("Test forAll function for Stream of Ints") {
    forAll (minSuccessful(8000), maxDiscarded(300)){ xs: Seq[Int] =>
      val actual  = Stream.apply(xs:_*)
       actual.forAll(p) should be (xs.forall(p))
    }
  }

  val f: Int => Int = x =>  x 
  property("Test map function for Stream of Ints") {
    forAll { xs: Seq[Int] =>
      val actual  = Stream.apply(xs:_*)
       (actual.map(f).toList) should be (xs.map(f))
    }
  }

    property("Test append function for Stream of Ints") {
    forAll {(xs: Seq[Int], ys: Seq[Int]) =>
      val a1  = Stream.apply(xs:_*)
      val a2  = Stream.apply(ys:_*)
      val expected = xs ++ ys
       (a1.append(a2).toList) should be (expected)
    }
  }


  val g: Int => Stream[Int] = x => Stream.cons(x, Stream.empty)

  property("Test flatMap function for Stream of Ints") {
    forAll { xs: Seq[Int] =>
      val actual  = Stream.apply(xs:_*)
      val mine = actual.flatMap(g).toList
      val theirs = xs.map(f).toList
      (mine) should be (theirs)
    }
  }

  property("Test filter function for Stream of Ints") {
    val even: Int => Boolean = x => x % 2 == 0
    forAll { xs: Seq[Int] =>
      val actual  = Stream.apply(xs:_*)
      val mine = actual.filter(even).toList
      val theirs = xs.filter(even).toList
      (mine) should be (theirs)
    }
  }

  property("Test constant function for Int") {
     forAll { x: Int =>
       val actual  = Stream.constant(x).take(4).toList
       val expected = List(x, x, x, x)
      actual should be (expected)
    }
  }

    property("Test from function for Int") {
     forAll { x: Int =>
       val actual  = Stream.from(x).take(4).toList
       val expected = List(x, x+1, x+2, x+3)
      actual should be (expected)
    }
  }

  property("Test fib function take 1") {
     val actual = Stream.fib.take(1).toList
     actual should be (List(0))
  }

  property("Test fib function take 5") {
     val actual = Stream.fib.take(5).toList
     actual should be (List(0, 1, 1, 2, 3))
  }

   property("Test fib function take 7") {
     val actual = Stream.fib.take(7).toList
     actual should be (List(0, 1, 1, 2, 3, 5, 8))
  }

  property("Test fibme function take 7") {//This is the beginning of unfold
     val actual = Stream.fibme(Stream.empty[Int]).take(7).toList
     actual should be (List(0, 1, 1, 2, 3, 5, 8))
  }

  property("Test fibme function take 1") { //This is the beginning of unfold
     val actual = Stream.fibme(Stream.empty[Int]).take(1).toList
     actual should be (List(0))
  }

  val giveMeAOnesForever: Stream[Int] => Option[(Int, Stream[Int])] = s => Some((1, Stream.cons(1,s)))
  property("Test unfold to produce a stream of ones and take 1") {
    val actual = Stream.unfold(Stream.cons(1, Stream.empty))(giveMeAOnesForever).take(1).toList
    actual should be (List(1))
  }

  property("Test unfold to produce a stream of ones and take 12 starting from an empty stream") {
    val actual = Stream.unfold(Stream.empty[Int])(giveMeAOnesForever).take(12).toList
    actual should be (List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
  }

  property("Test unfold to produce a stream of ones and take 12 starting from a stream of 1") {
    val actual = Stream.unfold(Stream.cons(1, Stream.empty))(giveMeAOnesForever).take(12).toList
    actual should be (List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
  }

  property("Test unfold with computation that stops when A is > 100") {
    val actual = Stream.unfold(0)(nextPositiveIntLT100).take(12).toList
    actual should be (List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
  }

  property("Test unfold with computation that stopping computation") {
    val actual = Stream.unfold(99)(nextPositiveIntLT100).take(12).toList
    actual should be (List(99))
  }

  property("Test unfold with computation that will stop when you take it too far") {
    val actual = Stream.unfold(100)(nextPositiveIntLT100).take(12).toList
    actual should be (empty)
  }

  property("Test unfold with computation that will stop") {
    val actual = Stream.unfold(100)(nextPositiveIntLT100).toList
    actual should be (empty)
  }

  property("Test unfold to get whole list based upon computation") {
    val actual = Stream.unfold(0)(nextPositiveIntLT100).toList
    actual should be (0 to 99)
  }


  val nextPositiveIntLT100: Int => Option[(Int, Int)] = s =>
     if (s < 100) Some(s, s + 1)
     else None 


  val fib: ((Int, Int)) => Option[(Int, (Int,Int))] = s => s match {
    case (0, 0) => Some(0, (1, 0))
    case _ => Some(s._1 + s._2, (s._2, s._1 + s._2))
  }

  property("Use unfold with fib") {
    val actual = Stream.unfold((0,0))(fib).take(7).toList
     actual should be (List(0, 1, 1, 2, 3, 5, 8))
  }

  property("Get 43rd fib number using unfold") {
     val fib43 =  433494437
     val actual = Stream.unfold((0,0))(fib).take(44).toList(43)
     actual should be (fib43)
  }

  property("Make a stream of constants  using unfold") {
    forAll { x: Int =>
      val constant: Int => Option[(Int, Int)] = s => Some(s, s)
      val actual = Stream.unfold(x)(constant).take(44).toList(43)
      actual should be (x)
    }
  }

  property("Make a stream of ones using unfold") {
    forAll {x: Int =>   // Start value does not matter
      val ones: Int => Option[(Int, Int)] = _  => Some(1, 1)
      val actual = Stream.unfold(x)(ones).take(44).toList(43)
      actual should be (1)
    }
  }

  property("Make a stream from some start using unfold") {
    forAll {x: Int =>  
      val from: Int => Option[(Int, Int)] = x  => Some(x, x + 1)
      val actual = Stream.unfold(x)(from).take(4).toList
      actual should be (List(x, x+1, x+2, x + 3))
    }
  }

  def mapWunfold[A, B](xs: Stream[A])(f: A => B): Stream[B] = Stream.unfold(xs)(x => x match {
    case  Cons(h, t) => Some(f(h()), t())
    case _ => None
    }
  )

  property("Write map using unfold") {
    forAll {xs: Seq[Int] =>  
      val st  = Stream.apply(xs:_*)
  
      val actual = mapWunfold(st)(f).toList
      actual should be (xs.map(f))
    }
  }

  def takeWunfold[A](n: Int) (xs: Stream[A]): Stream[A] =
    Stream.unfold((n, xs))(nxs => nxs match {
      case (n, Cons(h, t)) if (n > 0) => Some((h(), (n-1, t())))
      case _ => None
    }
  )
  

  property("Write take using unfold fixed size") {
      val xs = Seq(1,2,3,4,5)
      val st = Stream.apply(xs:_*)
      val actual = takeWunfold(2)(st).toList
      val expected = xs.take(2)
      actual should be (expected)
  }

  property("Write take using unfold") {
     forAll {(xs: Seq[Int], x: Int) =>
       val st = Stream.apply(xs:_*)
       val actual = takeWunfold(x)(st).toList
       val expected = xs.take(x)
       actual should be (expected)
     }
  }
}
