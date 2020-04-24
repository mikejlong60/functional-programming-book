package chapter11

import org.scalacheck._
import Prop.{forAll, propBoolean}

object  PolymorphicTest extends Properties("Polymorphic Monad test") {

  import Monad._

  def filterMTest[F[_]](mon: Monad[F]) =
    forAll{  (xs: List[Int]) =>
      val f =  (a: Int) => if (a > 1000) mon.unit(true) else mon.unit(false)
      val actual = mon.filterM(xs)(f)
      val expected = xs.filter(x => x > 1000)
      mon.map(actual)(a => a == expected)
      true
    }

  def joinTest[F[_]](mon: Monad[F]) =
    forAll{  (x: Int) =>
      val mma = mon.unit(mon.unit(x))
      println(mma)
      val actual = mon.join(mma)
      val expected = mon.unit(x)
      actual == expected
    }

  property("flatMapWithJoin for Stream") =
    forAll{  (x: Int) =>
      val f =  (a: Int) => if (a > 1000) streamMonad.unit(true) else streamMonad.unit(false)
      val actual = streamMonad.flatMapWithJoin(streamMonad.unit(x))(f).toList
      val expected = f(x).toList
      actual == expected
    }

  def flatMapWithJoinTest[F[_]](mon: Monad[F]) =
    forAll{  (x: Int) =>
      val f =  (a: Int) => if (a > 1000) mon.unit(true) else mon.unit(false)
      val actual = mon.flatMapWithJoin(mon.unit(x))(f)
      val expected = f(x)
      actual == expected
    }

 property("Test flatMap built using map and join") = {
    flatMapWithJoinTest(listMonad)
    flatMapWithJoinTest(optionMonad)
//    flatMapWithJoinTest(parMonad)
    //flatMapWithJoinTest(IOMonad)
    //flatMapWithJoinTest(eitherMonad[Exception])
    //flatMapWithJoinTest(stateMonad[Int])
  }

  property("Test join where they are not functions") = {
    joinTest(listMonad)
    joinTest(optionMonad)
  }

  property("Test join for Par") = {
    val ex1 = java.util.concurrent.Executors.newFixedThreadPool(2)
    val ex2 = java.util.concurrent.Executors.newFixedThreadPool(2)
    import chapter7.nonblocking.Nonblocking.Par
    forAll{  (x: Int) =>
      val f =  (a: Int) => if (a > 1000) -1 else a
      val mma = parMonad.unit(parMonad.unit(f))
      val ma = parMonad.join(mma)
      val mfa = Par.run(ex1)(mma).get
      val actual2 = Par.run(ex2)(mfa).get(x)
      val actual1 = Par.run(ex2)(ma).get(x)
      actual1 == f(x)
      actual1 == actual2
    }
  }

  property("flatMapWithJoin for Par") = {
    val ex = java.util.concurrent.Executors.newFixedThreadPool(10)
    import chapter7.nonblocking.Nonblocking.Par
    forAll{  (x: Int) =>
      val f =  (a: Int) => if (a > 1000) parMonad.unit(true) else parMonad.unit(false)
      val actual = Par.run(ex)(parMonad.flatMapWithJoin(parMonad.unit(x))(f))
      val expected = Par.run(ex)(f(x))
      actual == expected
    }
  }

  property("Test filterM") = {
    filterMTest(listMonad)
    filterMTest(optionMonad)
    filterMTest(streamMonad)
    filterMTest(parMonad)
    filterMTest(IOMonad)
    //filterMTest(eitherMonad[List[Int]])
    //filterMTest(stateMonad[Int])
  }
}
