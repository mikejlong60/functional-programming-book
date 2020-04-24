package chapter11

import chapter5.Stream
import org.scalacheck._
import Prop.{forAll, propBoolean}

object  StreamTest extends Properties("Stream Monad test") {

  import Monad._
  val mon = streamMonad

  property("Test map") =
    forAll { xs: List[Int] =>
      val xxs = chapter5.Stream(xs:_*)
      val actual = mon.flatMap(xxs)(x => Stream.cons(x + 1, Stream.empty))
      val expected = xs.flatMap(x => List(x + 1))
      actual.toList == expected
    }


  property("Test Map Law") = {
    val o = Stream.cons(1, Stream.cons(2, Stream.cons(1, Stream.empty)))
    //Folliowing is same as map law but need to evaluate the stream to provew it
    val g = mon.map(o)(a => a).toList
    val h = o.toList
    mon.mapLaw(Stream.empty) & g == h
  }



  property("Test Associative Law") =
    forAll {xs: List[Int]  =>
      val xxs = chapter5.Stream(xs:_*)

      val f =  (x: Int) => Stream.cons(x.toString, Stream.empty)
      val g = (y: String) => Stream.cons(s"the number was: $y", Stream.empty)

      val l = mon.flatMap(mon.flatMap(xxs)(f))(g).toList
      val r = mon.flatMap(xxs)(a => mon.flatMap(f(a))(g)).toList
      l == r & mon.associativeLaw(Stream.empty)(f)(g)
    }

  property("Test Kleisli Associative Law") =
    forAll {xs: List[Int]  =>
      val xxs = chapter5.Stream(xs:_*)

      val f =  (x: Stream[Int]) => Stream.cons(x.toList.toString, Stream.empty)
      val g = (y: String) => Stream.cons(y.toString, Stream.empty)
      val h =   (z: String) => Stream.cons(s"the element of the list was: $z", Stream.empty)

      val lf = mon.compose(mon.compose(f, g), h)
      val rf = mon.compose(f,mon. compose(g, h))
      lf(xxs).toList == rf(xxs).toList
    }

 
  property("Prove identity laws using Kleisli composition") =
    forAll {xs: List[Int]  =>
      val xxs = chapter5.Stream(xs:_*)
      val f =  (xs: Stream[Int]) => xs.map(x => s"${x * 3}")
      val li = mon.compose(f, (b: String) => mon.unit(b))
      val ri = mon.compose((a: Stream[Int]) => mon.unit(a), f)
      li(xxs).toList.toString == ri(xxs).toList.toString
    }

  property("Test flatmap") =
    forAll { xs: List[Int] =>
      val xxs = chapter5.Stream(xs:_*)

      val actual = mon.flatMap(xxs)(x => Stream.cons(x + 1, Stream.empty))
      val expected = xs.flatMap(x => List(x + 1))
      actual.toList == expected
    }


  property("Test flatmap on unit value") = {
    val actual = mon.flatMap(Stream.empty)(x => Stream.empty)
    val expected = Stream.empty
    actual == expected
  }

  val f: String => Stream[Int] = (x: String) =>
  try {
    val c = x.toInt
    Stream.cons(c, Stream.empty)
  } catch {
    case e: Exception => Stream.empty
  }
  
  property("Test traverse over unparseable number") = {
    val xs = Stream.cons("12",Stream.cons("13a", Stream.empty)).toList
    val expected = Stream.empty
    val actual = mon.traverse(xs)(f)
    actual == expected
  }

  property("Test traverse") =
    forAll { xs: List[Int] =>
      val gx = xs.map(_.toString)
      val xxs = chapter5.Stream(gx:_*)
      val expected = xxs.flatMap(f)
      val actual = mon.traverse(xxs.toList)(f)
      actual.toList == List(expected.toList)
    }

  property("Test sequence for non-empty list. Sequence flattens the list by one level.") =
    forAll { l: List[Int] =>
      val ls = chapter5.Stream(l:_*)
      val ll = ls.map(x => Stream.cons(x, Stream.empty))
      val actual = mon.sequence(ll.toList)
      actual.toList == List(l)
    }


  property("Test sequence for empty list") = {
    val ll = Stream.empty
    val actual = mon.sequence(ll.toList)
    actual.toList == List(List())
  }
}

