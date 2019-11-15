package chapter10

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import MonoidInstances._


class FoldableInstancesTest extends PropSpec with PropertyChecks with Matchers {

  property("seq contatenate test") {
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.seq.concatenate(l)(stringMonoid)
      val expected = l.foldLeft("")(_ + _)
      actual should be (expected)
    }
  }

  property("list contatenate test") {
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.list.concatenate(l.toList)(stringMonoid)
      val expected = l.foldLeft("")(_ + _)
      actual should be (expected)
    }
  }

  property("Stream contatenate test") {
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.stream.concatenate(chapter5.Stream(l:_*))(stringMonoid)
      val expected = l.foldLeft("")(_ + _)
      actual should be (expected)
    }
  }

  property("seq foldMap test") {
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.seq.foldMap(l)(_.toString)(stringMonoid)
      val expected = l.foldLeft("")(_ + _)
      actual should be (expected)
    }
  }

  property("list foldMap test") {
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.list.foldMap(l.toList)(_.toString)(stringMonoid)
      val expected = l.foldLeft("")(_ + _)
      actual should be (expected)
    }
  }

  property("Stream foldMap test") {
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.stream.foldMap(chapter5.Stream(l:_*))(_.toString)(stringMonoid)
      val expected = l.foldLeft("")(_ + _)
      actual should be (expected)
    }
  }

  property("seq foldLeft test") {
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.seq.foldLeft(l)(stringMonoid.zero)(stringMonoid.op)
      val expected = l.foldLeft("")(_ + _)
      actual should be (expected)
    }
  }

  property("list foldLeft test") {
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.list.foldLeft(l.toList)(stringMonoid.zero)(stringMonoid.op)
      val expected = l.foldLeft("")(_ + _)
      actual should be (expected)
    }
  }

  property("Stream foldLeft test") {
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.stream.foldLeft(chapter5.Stream(l:_*))(stringMonoid.zero)(stringMonoid.op)
      val expected = l.foldLeft("")(_ + _)
      actual should be (expected)
    }
  }

    property("seq foldRight test") {
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.seq.foldRight(l)(stringMonoid.zero)(stringMonoid.op)
      val expected = l.foldLeft("")(_ + _)
      actual should be (expected)
    }
  }

  property("list foldRight test") {
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.list.foldRight(l.toList)(stringMonoid.zero)(stringMonoid.op)
      val expected = l.foldLeft("")(_ + _)
      actual should be (expected)
    }
  }

  property("Stream foldRight test") {
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.stream.foldRight(chapter5.Stream(l:_*))(stringMonoid.zero)(stringMonoid.op)
      val expected = l.foldLeft("")(_ + _)
      actual should be (expected)
    }
  }

  property("Stream toList test") {
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.stream.toList(chapter5.Stream(l:_*))
      actual should be (l.toList)
    }
  }

property("Option foldRight test") {
    forAll{ o: Option[String] =>
      val actual = FoldableInstances.option.foldRight(o)("fred")((b, a) => a)
      val expected = o.foldRight("fred")((b, a) => a)
      println(actual)
      actual should be (expected)
    }
  }

  property("Option foldLeft test") {
    forAll{ o: Option[String] =>
      val actual = FoldableInstances.option.foldLeft(o)("")((b, a) => a ++ b)
      val expected = o.foldLeft("")((b, a) => a ++ b)
      println(actual)
      actual should be (expected)
    }
  }

    property("Option foldMap test") {
    forAll{ o: Option[String] =>
      val actual = FoldableInstances.option.foldMap(o)(a => a)(stringMonoid)
      val expected = o.foldLeft("")((b, a) => a ++ b)
      println(actual)
      actual should be (expected)
    }
  }


  //Fold tests for Tree

  import chapter3.{Tree, Branch, NilNode, Leaf}
  val f:(Int => Int) = b => b
  val g:((Int,Int) => Int) = (b1: Int, b2: Int) => math.max(b1, b2)
  val h:Int = Int.MinValue

  property("Test foldLeft maximum on empty tree ") {
    val t = Branch(NilNode, NilNode)
    val actual = FoldableInstances.tree.foldLeft(t)(h)(g)
    actual should be (Int.MinValue)
  }

  property("Test foldLeft maximum with Tree of 2 leaves of Ints") {
    val t = Branch(Leaf(1200), Leaf(2))
    val actual = FoldableInstances.tree.foldLeft(t)(h)(g)
    actual should be (1200)
  }

  property("Test foldRight maximum on empty tree ") {
    val t = Branch(NilNode, NilNode)
    val actual = FoldableInstances.tree.foldRight(t)(h)(g)
    actual should be (Int.MinValue)
  }

  property("Test foldRight maximum with Tree of 2 leaves of Ints") {
    val t = Branch(Leaf(1200), Leaf(2))
    val actual = FoldableInstances.tree.foldRight(t)(h)(g)
    actual should be (1200)
  }

  property("Test foldMap stringMonoid on empty tree ") {
    val t = Branch(NilNode, NilNode)
    val actual = FoldableInstances.tree.foldMap(t)(_.toString)(stringMonoid)
    actual should be ("")
  }

  property("Test foldMap stringMonoid with Tree of 2 leaves of Ints") {
    val t = Branch(Leaf(1200), Leaf(2))
    val actual = FoldableInstances.tree.foldMap(t)(_.toString)(stringMonoid)
    actual should be ("12002")
  }
}
