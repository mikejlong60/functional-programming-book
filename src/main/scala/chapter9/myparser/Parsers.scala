package chapter9
package myparser

import chapter9.myparser.ReferenceTypes.{Parser, ParseError}

object ReferenceTypes {
  type Parser[+A] = String => Result[A]
  type ParseError = String
}

trait Result[+A]
case class Success[+A](get: A) extends Result[A]
case class Failure(get: ParseError) extends Result[Nothing]

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }
}


object Reference extends Parsers[ParseError, Parser]  {

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???
  def char(c: Char): Parser[Char] = ???
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] = ???
  def string(s: String): Parser[String] = ??? //s => 

}
