package chapter10

import MonoidInstances._
import org.scalacheck._
import Prop.{forAll, propBoolean}

object  MonoidInstancesTest extends Properties("Monoid Instances test") {

  def zeroLawTest[A](m: Monoid[A])(x: A): Boolean = {
    val r=  m.zeroLaw(x)
    if (!r) false//println(s"x failed and was: $x")
    r == true
  }
  def associativeLawTest[A](m: Monoid[A])(x: A, y: A, z: A): Boolean = m.associativeLaw(x, y, z) == true

  property("Compute a bag from an IndexedSeq") = {
    val  bag = (xs: IndexedSeq[String]) =>  {
      val m:Monoid[Map[String, Int]]  = mapMergeMonoid(intAddition)
      FoldableInstances.seq.foldLeft(xs)(m.zero)((b,a) =>m.op(Map(a -> 1),b))
    }
    forAll{ xs: IndexedSeq[String] =>
      val actual = bag(xs)
      val expected = xs.toSet
      actual.keySet == expected
    }
  }

  property("Map merge Monoid test") =
    forAll{ (x: Map[String, Map[String, Int]], y: Map[String, Map[String, Int]]) =>
      val m: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))
      val actual = m.op(x, y)
      actual.keySet == (x.keySet ++ y.keySet)
    }

  property("Map merge monoid associative law test") =
    forAll{ (x: Map[String, Map[String, Int]], y: Map[String, Map[String, Int]], z: Map[String, Map[String, Int]]) =>
      val m: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))
      associativeLawTest(m)(x, y, z)
    }

  property("Map merge monoid zero law test") =
    forAll{ x: Map[String, Map[String, Int]] =>
      val m: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))
      zeroLawTest(m)(x)
    }

  property("function Monoid associative law") =
    forAll{ x: Int =>
      val sm = stringMonoid
      val f1 = (x: Int) => x.toString
      val f2 = (x: Int) => (x-12).toString
      val f3 = (x: Int) => (x + 12).toString
      val fm: Monoid[Int => String] = functionMonoid(sm)

      val op1= fm.op(fm.op(f1, f2) , f3)
      val op2 = fm.op(f1, fm.op(f2, f3))
      op1(x) == op2(x)
    }

  property("function Monoid zero law") =
    forAll{ x: Int =>
      val sm = stringMonoid
      val f1: Int => String  = (x: Int) => x.toString
      val fm: Monoid[Int=> String] = functionMonoid(sm)
      val z = fm.op(f1, fm.zero)
      z(x) == x.toString
    }

  property("function monoid composition concatenate with string monoid") =
    forAll{ x: Int =>
      val s1 = stringMonoid
      val fm: Monoid[Int => String] = functionMonoid(s1)
      val f1: Int => String  = (x: Int) => x.toString
      val f2: Int => String  = (x: Int) => (x-12).toString

      val f: Int => String  = fm.op(f1, f2)
      val actual = f(x)
      actual == (s"${x}${x-12}")
    }

  property("Product Monoid associative law") =
    forAll{(x1: (String, Int), x2: (String, Int), x3: (String, Int)) =>
      val s1 = stringMonoid
      val i1 = intMultiplication
      val p1 = productMonoid(s1, i1)

      associativeLawTest(p1)(x1, x2, x3)
    }

  property("Product Monoid zero law") =
    forAll{ x: (String, Int) =>
      val s1 = stringMonoid
      val i1 = intMultiplication
      val p1 = productMonoid(s1, i1)
      zeroLawTest(p1)(x)
    }

  property("Understand concatenate with product monoid") =
    forAll{ x: IndexedSeq[(String, Int)] =>
      val s1 = stringMonoid
      val i1 = intMultiplication
      val p1 = productMonoid(s1, i1)

      val (s, n) = x.unzip
      val ss = s.foldRight("")(_ ++ _)
      val nn = n.foldRight(1)((a, b) => a * b )
      val r = Monoid.concatenate(x)(p1)
      r == ((ss, nn))
    }

  property("String Monoid associative law") =
    forAll{(x: String, y: String, z: String) =>
      associativeLawTest(stringMonoid)(x, y, z)
    }

  property("String Monoid zero law") =
    forAll{ x: String =>
      zeroLawTest(stringMonoid)(x)
    }

  property("WC Monoid associative law with Part") =
    forAll{ (b:(String, Int, String), c: (String, Int,  String), d: (String, Int,  String)) =>
      val xx = Part(b._1, b._2, b._3)
      val yy = Part(c._1, c._2, c._3)
      val zz = Part(d._1, d._2, d._3)
      associativeLawTest(wcMonoid)(xx, yy, zz)
    }

  property("WC Monoid associative law with Stub") =
    forAll{(a: (String, String, String)) =>
      val x = Stub(a._1)
      val y = Stub(a._2)
      val z = Stub(a._3)
      associativeLawTest(wcMonoid)(x, y, z)
    }

  property("WC Monoid zero law with Stub") =
    forAll{ a: String =>
      val x = Stub(a)
      zeroLawTest(wcMonoid)(x)
    }

  property("WC Monoid zero law with Part") =
    forAll{ b: (String, Int, String) =>
      val xx = Part(b._1, b._2, b._3)
      zeroLawTest(wcMonoid)(xx)
    }

  property("Count the number of words in a long list of words") =
    forAll{ w: List[String] =>
      val words = w.mkString(" ")
      //println("original:" + w)
      //println(s"words: [$words]")
      val count  = countFromBook(words)
      //println(count)
      //println(w.size)
      if (words.replaceAll("\\s", "").isEmpty) count == 0
      else count == w.size
    }

  def countFromBook(s: String): Int = {//TODO Fix the bug in this
    // A single character's count. Whitespace does not count,
    // and non-whitespace starts a new Stub.
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)
    // `unstub(s)` is 0 if `s` is empty, otherwise 1.
    def unstub(s: String) = s.length min 1
    Monoid.foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  property("Int Addition Monoid associative law") =
    forAll{(x: Int, y: Int, z: Int) =>
      associativeLawTest(intAddition)(x, y, z)
    }

  property("Int Addition Monoid zero law") =
    forAll{ x: Int =>
      zeroLawTest(intAddition)(x)
    }

  property("Boolean and Monoid associative law") =
    forAll{(x: Boolean, y: Boolean, z: Boolean) =>
      associativeLawTest(booleanAnd)(x, y, z)
    }

  property("Boolean and Monoid zero law") =
    forAll{ x: Boolean =>
      zeroLawTest(booleanAnd)(x)
    }

  property("Boolean or Monoid associative law") =
    forAll{(x: Boolean, y: Boolean, z: Boolean) =>
      associativeLawTest(booleanOr)(x, y, z)
    }

  property("Boolean or Monoid zero law") =
    forAll{ x: Boolean =>
      zeroLawTest(booleanOr)(x)
    }

  property("Endofunctor Monoid associative law") =
    forAll{(x: Int) =>
      val f1 = (x: Int) => x + x
      val f2 = (x: Int) => x + 12
      val f3 = (x: Int) => x + 50
      val op1 = endoMonoid.op(endoMonoid.op(f1, f2) , f3)
      val op2 = endoMonoid.op(f1, endoMonoid.op(f2, f3))
      op1(x) == op2(x)
    }

  property("Endofunctor Monoid zero law") =
    forAll{ x: Boolean =>
      val f1 = (x: Boolean) => x != true
      val z = endoMonoid.op(f1, endoMonoid.zero)
      z(x) == !x
    }

  property("Option Monoid associative law") =
    forAll{(x: Option[Boolean], y: Option[Boolean], z: Option[Boolean]) =>
      associativeLawTest[Option[Boolean]](optionMonoid)(x, y, z)
    }

  property("Option Monoid zero law") =
    forAll{ x: Option[Boolean] =>
      zeroLawTest[Option[Boolean]](optionMonoid)(x)
    }

  property("contatenate test") =
    forAll{ l: IndexedSeq[String] =>
      val actual = Monoid.concatenate(l)(stringMonoid)
      val expected = l.foldLeft("")(_ + _)
      actual == expected
    }

  val blubber = (x: Int) =>  s"We got ${(x  * .65)} pounds of whale oil from this whale which weighed in at $x pounds. \n"

  property("Make foldMap for type that does not have a monoid instance") =
    forAll{whales: IndexedSeq[Int] =>
      val expected = whales.foldLeft("")((b, a) => b +   blubber(a))
      val actual = Monoid.lFoldMap(whales)(stringMonoid)(blubber)
      actual == expected
   }

  property("Write foldLeft using foldMap") =
    forAll{whales: IndexedSeq[Int] =>
      val expected = whales.foldLeft("")((b, a) => b +   blubber(a))
      val actual = Monoid.foldLeft(whales)(stringMonoid)(blubber)
      actual == expected
    }

    property("Write foldRight using rFoldMap") =
    forAll{whales: IndexedSeq[Int] =>
      val expected = whales.foldRight("")((a, b) => b +   blubber(a))
      val actual = Monoid.foldRight(whales)(stringMonoid)(blubber)
      actual == expected
    }

  property("Write splitting foldMap") =
    forAll{ xs: IndexedSeq[Int] =>
      val expected = xs.foldLeft(0)((b, a) => (a * 10) + b)

      val actual = Monoid.foldMapV(xs, intAddition)(a => a * 10)
      actual == expected
    }

  property("Make a parallel version of foldMap") = {
    import chapter7.nonblocking.Nonblocking.Par
    val es = java.util.concurrent.Executors.newFixedThreadPool(8)

    forAll {xs: IndexedSeq[Int] =>
      val expected = chapter7.nonblocking.Try(xs.sum * 100)
      val startTs = System.currentTimeMillis
      val actual = Par.run(es)(Monoid.parFoldMap(xs, intAddition)(a => a * 100))
      actual == expected
    }
  }

  property("Int Ordered Monoid associative law") =
   forAll{ (xs1:Option[ (Int, Int, Boolean)], xs2: Option[(Int, Int, Boolean)], xs3: Option[(Int, Int, Boolean)]) =>
     associativeLawTest(intOrdered)(xs1, xs2, xs3)
   }

  property("Int Ordered Monoid zero law") =
    forAll{ x: Option[(Int, Int, Boolean)] =>
      zeroLawTest(intOrdered)((x))
    }

  val foldMapIntOrdered = (ints: IndexedSeq[Int]) => Monoid.lFoldMap(ints)(intOrdered)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  property("Write a foldMap to detect if a given sequence is ordered") =
    forAll {xs: IndexedSeq[Int] =>
      val sorted = xs.sorted
      val alreadySorted = sorted == xs

      val myIsSorted = foldMapIntOrdered(xs)//.lFoldMap(xs)(mySlightlyWrongintOrdered)(a => (true, a))
      myIsSorted == alreadySorted

      val myAlreadySorted = foldMapIntOrdered(sorted)//Monoid.lFoldMap(sorted)(mySlightlyWrongintOrdered)(a => (true, a))
      myAlreadySorted == true
    }

  property("Show that the parellel version of foldMap produces much better throughput given the limitation on list size. See the comment on sequenceBalanced in chapter7.nonblocking.NonBlocking") = {
    import chapter7.nonblocking.Nonblocking.Par
    val es = java.util.concurrent.Executors.newFixedThreadPool(8)

    val xs =(1 to 100)
    val start1 = System.currentTimeMillis
    val actual = Par.run(es)(Monoid.parFoldMap(xs, intAddition)(a => {
      Thread.sleep(1)
      a * 100
    }))
    val actual1 = System.currentTimeMillis() - start1
    val expectedSlow = xs.foldLeft(0)((b, a) => (a * 10) + b)
    val start2 = System.currentTimeMillis()

    val actualSlow = Monoid.foldMapV(xs, intAddition)(a => {
      Thread.sleep(1)
      a * 100
    })
    val actual2 = System.currentTimeMillis() - start2
    actual1 < actual2
  }
}
