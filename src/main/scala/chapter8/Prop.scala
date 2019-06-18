package chapter8

import chapter6.RNG
import chapter5.Stream

object types {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type PropName = String
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

case class Prop(run:  (TestCases, RNG) => Result, name: PropName )  {

  def &&(p: Prop): Prop = Prop (
     run = (n,  rng) => run(n, rng) match {
      case Passed => p.run(n, rng)
       case Falsified(name, failure, successes) =>  Falsified(name, failure, successes)
     },
       name = this.name
  )

  def ||(p: Prop): Prop = Prop (
    run = (n, rng) => run(n, rng) match {
      case Passed => Passed
      case Falsified(_, failure, successes) => p.run(n, rng) 
    },
    name = this.name

  )
}
 //   val r1 = Prop {()}
  // if (check(p1.run()) && p2.check)
  //    Prop {
  //      def check = true
  //    }
  //    else {
  //      new PropT {
  //        def check = false
  //      }
  //    }


 // def check: Boolean
//  def &&(p: Prop): Prop =
//    if (p.check && this.check)
//      new PropT {
//        def check = true
//      }
//      else {
//        new PropT {
//          def check = false
//        }
//      }

object Prop {
  def forAll[A](as: Gen[A], name: PropName = "First")(f: A => Boolean): Prop = Prop (
    run = (n, rng) => {
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


//  def check(p: => Boolean): Prop = Prop { ( _, _) =>
//      if (p) Passed else Falsified(0, "()", 0)
//  }

}
