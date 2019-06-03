package chapter5

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.scalacheck.Gen

case class ODriveDoc(name: String)

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
  var first100 = 0

  property("Test a pipeline of functions fed by an unfold") {
     val asssctual = Stream.unfold(getNextDoc())(keepItRollingUntilNoMoreUnmigratedDocs).map(d => {
   //   println(d)
     s"$d:dude" 
    }).map(d => {
      val f = s"$d:mama"
//      //println(f)
      f
    })//.take(300).toList
    //actual should be (empty)
      (1 to 6000).map(n =>{
        val actual = Stream.unfold(getNextDoc())(keepItRollingUntilNoMoreUnmigratedDocs)
          .map(d => {
              s"$d:dude"
          })
          .map(d => {
              val f = s"$d:mama"
              f
            })
        val drain = actual.take(3000).toList
        //println(drain)
          
       // println(actual)
       /// first100 = 0
        //println(actual.take(300).toList)
//        val r = actual.head//take(30)//.toList
//       println(r)
        drain should have size(3000)
      })
   //actual should have size (1100)
  }

//    property("Test another pipeline of functions fed by an unfold") {
//      val a1 = Stream.unfold(getNextDoc())(keepItRollingUntilNoMoreUnmigratedDocs)
//      val a2 = mapWunfold(a1)(d => {
//        println(d)
//      s"$d:dude" 
//    })
//     val a3 =  mapWunfold(a2)(d => {
//      val f = s"$d:mama"
//      f
//     })//.take(30000).toList
//       (1 to 100000).map(n => {
//         val x = a3.take(1)
//         //println(x)
//         x
//       })
    //a3 should have size (100)
 // }

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

  val keepItRollingUntilNoMoreUnmigratedDocs: Option[ODriveDoc] => Option[(ODriveDoc, Option[ODriveDoc])] = doc => doc match {
    case Some(d) => Some(d, getNextDoc())
    case None => None
  }


  val getNextDoc: () => Option[ODriveDoc] = () => {
    first100 = first100 + 1
    if (first100 < 1000000001)
      Some(ODriveDoc(s"fred:$first100"))
    else None
  }


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

  def takeWhileWunfold[A](xs: Stream[A])(p: A => Boolean): Stream[A] = Stream.unfold(xs)(xs => xs match {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  })

  property("Write takewhile using unfold for Stream of ints") {
    forAll { xs: Seq[Int] =>
     val st = Stream.apply(xs:_*)
     val actual = takeWhileWunfold(st)(p).toList
     actual should be (xs.takeWhile(p))
    }
  }

  def zipWunfold[A](xs: Stream[A])(ys: Stream[A])(f: (A, A)=> A):Stream[A] = Stream.unfold((xs, ys))(pr => pr match {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  })

  property("Write zipwith using unfold for Stream of ints") {
    forAll { (xs: Array[Int], ys: Array[Int]) =>
      val expected = (xs, ys).zipped.toList.map(xy => xy._1 + xy._2)
      val xxs = Stream.apply(xs:_*)
      val xys = Stream.apply(ys:_*)
      val actual = zipWunfold(xxs)(xys)((x,y) => x + y).toList
      actual should be (expected)
     }
  }

  def zipAllWunfold[A, B](xs: Stream[A])(ys: Stream[B]):Stream[(Option[A], Option[B])] = Stream.unfold((xs, ys))(pr => pr match {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), empty) => Some((Some(h1()), None), (t1(), empty))
    case (empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
    case _ => None
  })

  property("Write zipAll using unfold for Stream of ints") {
    forAll { (xs: Seq[Int], ys: Seq[Int]) =>
      val expected = xs.map(x => Some(x)).zipAll(ys.map(y => Some(y)), None, None)
      val xxs = Stream.apply(xs:_*)
      val xys = Stream.apply(ys:_*)
      val actual = zipAllWunfold(xxs)(xys).toList
      actual should be (expected)
     }
  }

  //This is the one I wrote before I read the directions fully. It passes the tests as well
  def hasSubsequenceNotAsGood[A](sup: Stream[A])(sub: Stream[A]):Boolean = (sup, sub) match {
    case (Cons(h1, t1), Cons(h2, t2)) => {
      if (takeWhileWunfold(zipAllWunfold(sup)(sub))(x  => !x._2.isEmpty).forAll(x2 => x2._1 == x2._2)) true
      else hasSubsequence(t1())(sub)
    }
    case _ => false
  }

  //This is the one I wrote after following the directions and writing tails and startsWith
  def hasSubsequence[A](sup: Stream[A])(sub: Stream[A]): Boolean = Stream.tails(sup) exists (s =>  startsWith(s)(sub)) 

  def startsWith[A](sup: Stream[A])(sub: Stream[A]):Boolean = (sup, sub) match {
    case (Cons(h1, t1), Cons(h2, t2)) => takeWhileWunfold(zipAllWunfold(sup)(sub))(x  => !x._2.isEmpty).forAll(x2 => x2._1 == x2._2)
    case _ => false
  }

  property("Write tails for Stream of Strings") {
    forAll { (xs: Seq[String]) =>
      val xss = Stream.apply(xs:_*)
      val actual = xss.tails.map((x => x.toList)).toList
      actual should be (xs.tails.toList)
    }
  }

  property("Write startsWith false for Stream of Strings") {
    forAll { (xs1: Seq[String], xs2: Seq[String]) =>
      whenever (xs2.size > xs1.size) {
        val sup = Stream.apply(xs1:_*)
        val sub = Stream.apply(xs2:_*)
        (startsWith(sup)(sub)) should be (false)
      }
    }
  }

  property("Write startsWith true for Stream of Strings") {
    forAll { (xs: Seq[String]) =>
      whenever (xs.size > 200) {
        val sup = Stream.apply(xs:_*)
        val sub = Stream.apply(xs.take(100):_*)
        (startsWith(sup)(sub)) should be (true)
      }
    }
  }

  val ff = hasSubsequence _
  val gg  = hasSubsequenceNotAsGood _ 
  val hh = for {
    i <- Gen.oneOf(ff, gg)
  } yield (i)

  property("Write hasSubsequence false for Stream of Strings") {
    forAll { (xs1: Seq[String], xs2: Seq[String]) =>
      whenever (xs2.size > xs1.size) {
        val sup = Stream.apply(xs1:_*)
        val sub = Stream.apply(xs2:_*)
       (hh.sample.get(sup)(sub)) should be (false)
      }
    }
  }

  property("Write hasSubsequence true for Stream of ints") {
    forAll { xs: Seq[Int] =>
      whenever (xs.size >= 3) {
        val sub = Stream.apply(xs.takeRight(3):_*)
        val sup = Stream.apply(xs:_*)
        (hh.sample.get(sup)(sub)) should be (true)
      }
    }
  }

  property("Write hasSubsequence true for Stream of Strings") {
    forAll { xs: Seq[String] =>
      whenever (xs.size >= 3) {
        val sub = Stream.apply(xs.take(3):_*)
        val sup = Stream.apply(xs:_*)
        (hh.sample.get(sup)(sub)) should be (true)
      }
    }
  }

  property("Write scanRight for Stream of Ints") {
    forAll { (xs: Seq[Int]) =>
      val xss = Stream.apply(xs:_*)
      val actual = Stream.scanRight(xss)(Stream.empty[String])((a, s) => Stream.cons(a.toString(), s)).map(l => l.toList).toList
      val expected = xs.scanRight(List.empty[String])((a, s) => a.toString +: s)
      actual should be (expected)
    }
  }
}
