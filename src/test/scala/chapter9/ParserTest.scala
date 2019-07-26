package chapter9

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import chapter8.Gen
import chapter6.SimpleRNG

class ParserTest extends PropSpec with PropertyChecks with Matchers {

  //class MyParser[+A](p: String)
  //object MyParsers extends Parsers[MyParser] {
  //  def attempt[A](p: ParserTest.this.MyParser[A]): ParserTest.this.MyParser[A] = ???
  //  def flatMap[A, B](p: ParserTest.this.MyParser[A])(f: A => ParserTest.this.MyParser[B]): ParserTest.this.MyParser[B] = ???
  //  def label[A](msg: String)(p: ParserTest.this.MyParser[A]): ParserTest.this.MyParser[A] = ???
  //  def or[A](p1: ParserTest.this.MyParser[A],p2: => ParserTest.this.MyParser[A]): ParserTest.this.MyParser[A] = ???
  //  implicit def regex(r: scala.util.matching.Regex): ParserTest.this.MyParser[String] = ???
  //  def scope[A](msg: String)(p: ParserTest.this.MyParser[A]): ParserTest.this.MyParser[A] = ???
  //  def slice[A](p: ParserTest.this.MyParser[A]): ParserTest.this.MyParser[String] = ???
  //  def succeed[A](a: A): ParserTest.this.MyParser[A] = ???
  //  def run[A](p: MyParser[A])(input: String): Either[ParseError, A] = {
  //    println("piss")
      //if (p)
      //val f = a => input == 
      //this.map(p)(input == "asd")// => s)
   ///   Left(ParseError(List((Location("1st position"), "the heck"))))//"hello")/
      //p(input)

   // }
 // }
  val P = chapter9.instances.Reference
  import chapter9.instances.ReferenceTypes.Parser
  
  property("Prove the equal law for your parser") {
    forAll{(c: String, n: Int, done: Boolean, lots: List[String]) =>
      val goodJson = s"""{
"SomeString": "${c}", 
 "SomeNumber": $n,
"Done" : $done
}
"""
      val json: Parser[JSON] = JSON.jsonParser(P)
      val actual = P.run(json)(goodJson)

      println(actual)
      //val p1 = c.toString
      //val prop = MyParsers.Laws.equal(c.toString, c.toString)(Gen.unit(c.toString))
      //val rng = SimpleRNG(n)
      //val result = prop.run(100, 120, rng)
      //result should be (chapter8.Passed)
    }
  }

  //property("Prove the map law for your parser. This is busted because I have not implemented MyParsers") {
   //forAll{(c: Char, n: Int) =>
    //  val prop = MyParsers.Laws.mapLaw(c.toString)(Gen.unit(c.toString))
    //  val rng = SimpleRNG(n)
     // val result = prop.run(100, 120, rng)
     // result should be (chapter8.Passed)
  // }
  //}

  //property("Join parsers with `or` using ParserOps.  This is busted too because I have not implemented MyParsers") {
  //  forAll{(s1: String, s2: String, n: Int) =>
  //    val o = s1 | s2
  //    val prop = MyParsers.Laws.mapLaw(o)(Gen.unit(s1))
  //    val rng = SimpleRNG(n)
  //    val result = prop.run(100, 120, rng)
  //    result should be (chapter8.Passed)
  //  }
 // }
}

