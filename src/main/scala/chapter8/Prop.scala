package chapter8

import chapter6.RNG
import chapter5.Stream

object types {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type PropName = String
  type MaxSize = Int
}

import types._

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result  {
  def isFalsified = false
}

case class Falsified(name: PropName, failure: FailedCase, successes: SuccessCount) extends Result  {
  def isFalsified = true
}

case class Prop(run:  (MaxSize, TestCases, RNG) => Result, name: PropName = "First" )  {

  def &&(p: Prop): Prop = Prop (
     run = (max, n,  rng) => run(max, n, rng) match {
      case Passed => p.run(max, n, rng)
       case Falsified(name, failure, successes) =>  Falsified(name, failure, successes)
     },
       name = this.name
  )

  def ||(p: Prop): Prop = Prop (
    run = (max, n, rng) => run(max, n, rng) match {
      case Passed => Passed
      case Falsified(_, failure, successes) => p.run(max, n, rng) 
    },
    name = this.name

  )
}

object Prop {

 // def apply(f: (TestCases,RNG) => Result): Prop = Prop (run = (_,n,rng) => f(n,rng) )

  def forAll[A](as: Gen[A], name: PropName = "First")(f: A => Boolean): Prop = Prop (
    run = (max, n, rng) => {
      val r = Stream.zip(randomStream(as)(rng), Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(name, a.toString, i)
        } catch { case e: Exception => Falsified(name, buildMsg(a, e), i) }
      }
      Stream.find(r)(c => c.isFalsified).getOrElse(Passed)
    },
    name = name
  )

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
  s"generated an exception: ${e.getMessage}\n" +
  s"stack trace: \n ${e.getStackTrace.mkString("\n")}"


  //TODO Write some tests aroungd these SGen guys.
  def forAll[A](g: SGen[A], name: PropName)(f: A => Boolean) : Prop = forAll(g(_), name)(f)

  def forAll[A](g: Int =>  Gen[A], name: PropName)(f: A => Boolean): Prop = Prop (
    run  = (max, n, rng) => {
      val casesPerSize = (n + (max -1)) / max
      val props =  Stream.from(0).take((n min max) + 1)
      val c = props.map(i => forAll(g(i))(f))
      val prop: Prop = c.map(p => Prop { (max, _, rng) => p.run(max, casesPerSize, rng)}).toList.reduce(_ && _)
      prop.run(max, n, rng) 
    },
    name = name
  )

//  def check(p: => Boolean): Prop = Prop { ( _, _) =>
//      if (p) Passed else Falsified(0, "()", 0)
//  }

}
