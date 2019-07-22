package chapter9

import chapter8.{Gen, Prop}
import Prop._



case class ParseError(stack: List[(String, String)])

object Types {
  type Parser[+A] = String => Either[ParseError, A]
}

trait Parsers[Parser[+_]] { self =>
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2:  => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
  }

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString) map (s => s.charAt(0))
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]
  def succeed[A](a: A): Parser[A] = string("") map (s => a)

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))


  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = forAll(in, "Two Parsers of the same type should produce the same result when given the same argument.")(s => run(p1)(s) == run(p2)(s))

    //Mapping over id gives you back your original parser. Map preserves structure.
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
  }
}

//class CharParser[+A](s: String) extends Parsers[String, Char]
//object MyParsers extends Parsers[MyParser, String]
