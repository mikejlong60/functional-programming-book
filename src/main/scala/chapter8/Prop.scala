package chapter8

import chapter6.RNG
import chapter6.SimpleRNG
import chapter5.Stream
import java.util.concurrent._
import chapter7.Par.Par  //nonblocking.Nonblocking.Par

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

case object Proved extends Result  {
  def isFalsified = false
}

case class Prop(run:  (MaxSize, TestCases, RNG) => Result, name: PropName = "First" )  {

  def &&(p: Prop): Prop = Prop (
     run = (max, n,  rng) => run(max, n, rng) match {
      case Passed  | Proved => p.run(max, n, rng)
       case Falsified(name, failure, successes) =>  Falsified(name, failure, successes)
     },
       name = this.name
  )

  def ||(p: Prop): Prop = Prop (
    run = (max, n, rng) => run(max, n, rng) match {
      case Passed | Proved => Passed
      case Falsified(_, failure, successes) => p.run(max, n, rng) 
    },
    name = this.name

  )

  def check(p: => Boolean): Prop = Prop {(propName,  _,  _) =>
    if (p) Passed else Falsified("don't know propName yet", "()", 0)
  }
}

object Prop {

  def forAll[A](as: Gen[A], name: PropName = "First")(f: A => Boolean): Prop = Prop (
    run = (max, n, rng) => {
      val r = Stream.zip(randomStream(as)(rng), Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed  else Falsified(name, a.toString, i)
        } catch { case e: Exception => Falsified(name, buildMsg(a, e), i) }
      }
      Stream.find(r)(c => c.isFalsified).getOrElse(Passed)
    },
    name = name
  )

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"${Console.RED} test case: $s\n" +
  s"${Console.RED} generated an exception: ${e.getMessage}\n" +
  s"${Console.RED} stack trace: \n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A], name: PropName)(f: A => Boolean) : Prop = forAll(g(_), name)(f)

  def forAll[A](g: Int =>  Gen[A], name: PropName)(f: A => Boolean): Prop = Prop (
    run  = (maxSizeOfGenerator, numberOfTestCases, rng) => {
      val casesPerSize = (numberOfTestCases + (maxSizeOfGenerator -1)) / maxSizeOfGenerator
      val props =  Stream.from(0).take((numberOfTestCases min maxSizeOfGenerator) + 1)
      val c = props.map(i => forAll(g(i), name)(f))
      val prop: Prop = c.map(p => Prop { (max, _, rng) => p.run(max, casesPerSize, rng)}).toList.reduce(_ && _)
      prop.run(maxSizeOfGenerator, numberOfTestCases, rng) 
    },
    name = name
  )

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(propName, msg, n) => println(s"${Console.RED} ! Property [$propName] failed after $n passed tests: \n $msg")
      case Passed => println(s"${Console.GREEN} OK, passed $testCases tests.")
      case Proved => println(s"${Console.GREEN} OK,  proved property")
    }

  val S = Gen.weighted(Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> 75,
    Gen.unit(Executors.newCachedThreadPool) -> 25
  )

   object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S  ** g) { case (s ** a) => f(a)(s).get }
}
