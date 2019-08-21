package chapter9
package myparser

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import chapter8.Gen
import chapter6.SimpleRNG

class JSONParserTest extends PropSpec with PropertyChecks with Matchers {

  val P = chapter9.myparser.Reference
  import chapter9.myparser.ReferenceTypes.Parser
  import P.{string => _,_}

  
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

  property("Prove the map law for your parser.") {
    forAll{(s1: String, s2: String, n: Int) =>
      val o =  P.string(s1)  | P.string(s2)
      val prop = P.Laws.mapLaw(o)(Gen.unit(s1 ++ s2))
      val rng = SimpleRNG(n)
      val result = prop.run(100, 120, rng)
      result should be (chapter8.Passed)
    }
  }

  property("Prove the succeed law for your parser.") {
    forAll{(s1: String, s2: String, n: Int) =>
      val o =  P.string(s1)  | P.string(s2)
      val prop = P.Laws.succeedLaw(o)(Gen.unit(s1))
      val rng = SimpleRNG(n)
      val result = prop.run(100, 120, rng)
      result should be (chapter8.Passed)
    }
  }

  property("Prove the label law for your parser.") {
    forAll{(s1: String, s2: String, n: Int) =>
      val o =  P.string(s1)  | P.string(s2)
      val prop = P.Laws.labelLaw(o, Gen.unit(s1), Gen.unit(s2))
      val rng = SimpleRNG(n)
      val result = prop.run(100, 120, rng)
      result should be (chapter8.Passed)
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
      val a1: Either[ParseError, JSON] = P.run(g)(json)
      a1 should be (Right(JSON.JNull))
    }
  }
}

