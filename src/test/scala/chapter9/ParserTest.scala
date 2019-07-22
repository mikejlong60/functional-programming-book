package chapter9

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import chapter8.Gen
import chapter6.SimpleRNG

import chapter9.Types._

class ParserTest extends PropSpec with PropertyChecks with Matchers {

  class MyParser[+A](p: String)
  object MyParsers extends Parsers[MyParser] {
    def listOfN[A](n: Int,p: MyParser[A]): MyParser[List[A]] = ???
    def map[A, B](a: MyParser[A])(f: A => B): MyParser[B] = {
      val p = ParserOps(a).map(f)
      p
    }
    
    def or[A](s1: MyParser[A],s2: MyParser[A]): MyParser[A] = ???
    def run[A](p: MyParser[A])(input: String): Either[ParseError, A] = Left(ParseError(List(("what", "the heck"))))//"hello")//p//(input)
    def string(s: String): MyParser[String] = new MyParser(s)
  }

  val P: Parsers[MyParser] = MyParsers
 
  import P._

  property("Prove the equal law for your parser") {
    forAll{(c: Char, n: Int) => 
      val prop = MyParsers.Laws.equal(c.toString, c.toString)(Gen.unit(c.toString))
      val rng = SimpleRNG(n)
      val result = prop.run(100, 120, rng)
      result should be (chapter8.Passed)
    }
  }

  property("Prove the map law for your parser. This is busted because I have not implemented MyParsers") {
   forAll{(c: Char, n: Int) =>
      val prop = MyParsers.Laws.mapLaw(c.toString)(Gen.unit(c.toString))
      val rng = SimpleRNG(n)
      val result = prop.run(100, 120, rng)
      result should be (chapter8.Passed)
   }
  }

  property("Join parsers with `or` using ParserOps.  This is busted too because I have not implemented MyParsers") {
    forAll{(s1: String, s2: String, n: Int) =>
      val o = s1 | s2
      val prop = MyParsers.Laws.mapLaw(o)(Gen.unit(s1))
      val rng = SimpleRNG(n)
      val result = prop.run(100, 120, rng)
      result should be (chapter8.Passed)
    }
  }
}

