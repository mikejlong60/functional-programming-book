package chapter9

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

import chapter9.Types._

class ParserTest extends PropSpec with PropertyChecks with Matchers {

  property("Run the char parser") {
    forAll{c: Char => {
      val hasAnAParser = (i: String) =>  {
        if (i.contains("a")) Right(i)
        else Left(ParseError(List((i, "Not an a"))))
      }

      class MyParser[+A](p: String)
      object MyParsers extends Parsers[MyParser] {
     // val stringParser2 = (c:Char => Either[Error, String])  = Right(c)
     //val parsers = new Parsers [String, Parser] {
        def listOfN[A](n: Int,p: MyParser[A]): MyParser[List[A]] = ???
        def map[A, B](a: MyParser[A])(f: A => B): MyParser[B] = ???
        def or[A](s1: MyParser[A],s2: MyParser[A]): MyParser[A] = ???
        def run[A](p: MyParser[A])(input: String): Either[ParseError, A] = Left(ParseError(List(("what", "the heck"))))//"hello")//p//(input)
        def string(s: String): MyParser[String] = new MyParser(s)
      }

     // }
      val actual = MyParsers.run(new MyParser(c.toString))(c.toString)
      val expected = Right(c)
     // actual should be expected
    }


    }
  }
}


