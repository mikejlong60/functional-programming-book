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

  
  property("Practice parsing some JSON") {
    forAll{(c: String, n: Double, done: Boolean, lots: List[Float]) =>
      val goodJson = s"""{
"SomeString": "${c}", 
 "SomeNumber": $n,
"Done" : $done,
"Lots" : ${lots.mkString("[",",","]")},
"What" : null
}
"""
      val goodJson2 = s"""{"MikeObject" : $goodJson}"""
      val p: Parser[JSON] = JSON.jsonParser(P)
      val actual = P.run(p)(goodJson)
      val actual2 = P.run(p)(goodJson2)
      actual shouldBe a [Right[_, _]]
      actual2 shouldBe a [Right[_,_]]
    }
  }

 // import chapter8._
 // import chapter8.Prop._

 // object Laws {
 //   def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
 //     forAll(in)(s => run(p1)(s) == run(p2)(s)/)

//    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
 //     equal(p, p.map(a => a))(in)
 // }


  property("Prove the map law for your parser.") {
    forAll{(c: Int, n: Double, done: Boolean, lots: List[Float]) =>
      val json = s"""{
"SomeString": "${c}", 
 "SomeNumber": $n,
"Done" : $done,
"Lots" : ${lots.mkString("[",",","]")},
"What" : null
}
"""
      val p1 = JSON.jsonParser(P)
      val p2=  P.map(p1)(x => x)
      val a1 = P.run(p1)(json)
      val a2 = P.run(p2)(json)
      a1 should be (a2)
    }
  }

  property("Verify or behavior") {
    forAll{(c: Int, n: Double, done: Boolean, lots: List[Float]) =>
      val json = s"""{
"SomeString": "${c}", 
 "SomeNumber": $n,
"Done" : $done,
"Lots" : ${lots.mkString("[",",","]")},
"What" : null
}
"""
      val p1 = P.map(JSON.jsonParser(P))(x => JSON.JNull)
      val p2=  P.map(p1)(x => JSON.JString(n.toString))
      val g = P.or(p1,p2)
      val a1 = P.run(g)(json)
      a1 should be ("fred")
    }
  }


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

