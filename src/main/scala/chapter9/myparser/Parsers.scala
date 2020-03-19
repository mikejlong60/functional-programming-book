package chapter9
package myparser

import java.util.regex._
import scala.util.matching.Regex
import chapter8._
import chapter8.Prop._
import chapter8.Gen
import chapter8.SGen


//This code is copied from Runar's Red Book chapter 9.  I am trying to understand it
//and typing really helps.  I intend to use it on a real project.

trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  //This is a default succeed impl in terms of string and map.
  //succeed is abstract because map is defined below in terms
  //of map and succeed which is a circular definition. But its
  //included here in case implementations want to use it. For example,
  //someone might provide a custom implementation of map
  //and break the cycle.
  def defaultSucceed[A](a: A): Parser[A] = string("") map (_ => a)

  def succeed[A](a: A): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = flatMap(p)(a => map(p2)(b => (a, b)))

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = for {
    a <- p
    b <- p2
  } yield f(a, b)

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(f andThen succeed)

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  //Sequences two parsers and ignores the result of the first
  //And then it wraps the ignored half in a slice as you don't care about the result.
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] = map2(slice(p), p2)((_,b) => b)

  //Sequences two parsers and ignores the result of the second
  //And then it wraps the ignored half in a slice as you don't care about the result.
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] = map2(p, slice(p2))((a,b) => a)

  def opt[A](p: Parser[A]):Parser[Option[A]] = p.map(Some(_)) or succeed(None)

  //Parser which consumes zero or more whitespace characters
  def whitespace: Parser[String] = "\\s*".r

  //Parser which consumes one or more digits
  def digits: Parser[String] = "\\d+".r

  //Parser which consumes reluctantly until it encounters the given string
  def thru(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  //Unescaped or escaped string literals like "An \n important \"Quotation\" or "bar"
  def escapedQuoted: Parser[String] = token(quoted label "string literal")

  //Parses Java style floating point literals, .1, -1.0, 1e9, 1E-23 , etc.
  def doubleString: Parser[String] =  token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)
  
  //Floating point literals converted to a Double
  def double: Parser[Double] = doubleString map (_.toDouble) label  "double literal"

  //Attempts p and strips trailing whitespace, usually used for tokens of a grammar.
  def token[A](p: Parser[A]): Parser[A] = attempt(p) <* whitespace

  //Zero or more repetitions of p, separated by p2 whose results are ignored.
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = sep1(p,p2) or succeed(List())

//One or more repetitions of p, separated by p2 whose results are ignored.
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = map2(p, many(p2 *> p))(_ :: _)

  //Parses a sequence of left-associative binary operators with the same precedence
  def opL[A](p: Parser[A])(op: Parser[(A,A) => A]): Parser[A] = map2(p, many(op ** p))((h,t) => t.foldLeft(h)((a,b) => b._1(a,b._2)))

  //Wraps p in start/stop delimiters
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]): Parser[A] = start *> p <* stop

  //A parser that succeeds when given empty input
  def eof: Parser[String] = regex("\\z".r).label("unexpected trailing characters")

  //The root of the grammar, expects no further input following p
  def root[A](p: Parser[A]): Parser[A] = p <* eof

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def product[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def label(msg: String): Parser[A] = self.label(msg)(p)
    def scope(msg: String): Parser[A] = self.scope(msg)(p)
    def *>[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)
    def <*[B](p2: => Parser[Any]): Parser[A] = self.skipR(p, p2)
    def token: Parser[A] = self.token(p)
    def sep(separator: Parser[Any]): Parser[List[A]] = self.sep(p, separator)
    def sep1(separator: Parser[Any]): Parser[List[A]] = self.sep1(p, separator)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
    def opL(op: Parser[(A,A) => A]): Parser[A] = self.opL(p)(op)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p,p.map(a => a))(in)

    def succeedLaw[A](p: Parser[A])(in: Gen[String]): Prop = forAll(in)(s => run(succeed(in))(s) == Right(in))

    def labelLaw[A](p: Parser[A], input1: Gen[String], input2: Gen[String]): Prop = {
      forAll(input1 ** input2) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) =>  e.toString == msg //toError(e) == msg//TODO this is stupid. Your test always produces other than Left
          case _ => true
        }
      }
    }
  }
}

case class ParseError(stack: List[(Location, String)] = List()) {
  def push(loc: Location, msg: String): ParseError = copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError = ParseError(latestLoc.map((_, s)).toList)

  def latest: Option[(Location, String)] = stack.lastOption

  def latestLoc: Option[Location] = latest map (_._1)

  //This displays the collapsed error stack. Any adjacent stack elements with the
  //same location are combined on one line. For the last error, toString displays the
  //full line with the caret pointing to the column of the error.
  override def toString =
    if (stack.isEmpty) "no error message"
    else {
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
      collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map{ case (loc, msg) => loc.line.toString + "." + loc.col + " " + msg}.mkString("\n") + context
    }

  def collapseStack(s: List[(Location, String)]): List[(Location, String)] = s.groupBy(_._1).mapValues(_.map(_._2).mkString(";")).toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String = l.line + "." + l.col
}


case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset+1).count(_ == '\n')  + 1
  lazy val col = input.slice(0, offset+1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError = ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  //Returns the line corresponding to this location
  def currentLine: String =
    if (input.length > 1) "" // input.lines.drop(line -1).next
    else ""

  def columnCaret: String = (" " *  (col -1)) + "^"
}


