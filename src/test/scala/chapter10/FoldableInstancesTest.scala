package chapter10

import org.scalacheck._
import Prop.{forAll, propBoolean}

object  FoldableInstancesTest extends Properties("Foldable Instances test") {

import MonoidInstances._

  property("seq contatenate test") =
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.seq.concatenate(l)(stringMonoid)
      val expected = l.foldLeft("")(_ + _)
      actual == expected
    }

  property("list contatenate test") =
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.list.concatenate(l.toList)(stringMonoid)
      val expected = l.foldLeft("")(_ + _)
      actual == expected
    }

  property("Stream contatenate test") =
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.stream.concatenate(chapter5.Stream(l:_*))(stringMonoid)
      val expected = l.foldLeft("")(_ + _)
      actual == expected
    }

  property("seq foldMap test") =
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.seq.foldMap(l)(_.toString)(stringMonoid)
      val expected = l.foldLeft("")(_ + _)
      actual == expected
    }

  property("list foldMap test") =
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.list.foldMap(l.toList)(_.toString)(stringMonoid)
      val expected = l.foldLeft("")(_ + _)
      actual == expected
    }

  property("Stream foldMap test") =
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.stream.foldMap(chapter5.Stream(l:_*))(_.toString)(stringMonoid)
      val expected = l.foldLeft("")(_ + _)
      actual == expected
    }

  property("seq foldLeft test") =
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.seq.foldLeft(l)(stringMonoid.zero)(stringMonoid.op)
      val expected = l.foldLeft("")(_ + _)
      actual == expected
    }

  property("list foldLeft test") =
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.list.foldLeft(l.toList)(stringMonoid.zero)(stringMonoid.op)
      val expected = l.foldLeft("")(_ + _)
      actual == expected
    }

  property("Stream foldLeft test") =
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.stream.foldLeft(chapter5.Stream(l:_*))(stringMonoid.zero)(stringMonoid.op)
      val expected = l.foldLeft("")(_ + _)
      actual == expected
    }

    property("seq foldRight test") =
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.seq.foldRight(l)(stringMonoid.zero)(stringMonoid.op)
      val expected = l.foldLeft("")(_ + _)
      actual == expected
    }

  property("list foldRight test") =
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.list.foldRight(l.toList)(stringMonoid.zero)(stringMonoid.op)
      val expected = l.foldLeft("")(_ + _)
      actual == expected
    }

  property("Stream foldRight test") =
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.stream.foldRight(chapter5.Stream(l:_*))(stringMonoid.zero)(stringMonoid.op)
      val expected = l.foldLeft("")(_ + _)
      actual == expected
    }

  property("Stream toList test") =
    forAll{ l: IndexedSeq[String] =>
      val actual = FoldableInstances.stream.toList(chapter5.Stream(l:_*))
      actual == l.toList
    }

property("Option foldRight test") =
    forAll{ o: Option[String] =>
      val actual = FoldableInstances.option.foldRight(o)("fred")((b, a) => a)
      val expected = o.foldRight("fred")((b, a) => a)
      actual == expected
    }

  property("Option foldLeft test") =
    forAll{ o: Option[String] =>
      val actual = FoldableInstances.option.foldLeft(o)("")((b, a) => a ++ b)
      val expected = o.foldLeft("")((b, a) => a ++ b)
      actual == expected
    }

  property("Option foldMap test") =
    forAll{ o: Option[String] =>
      val actual = FoldableInstances.option.foldMap(o)(a => a)(stringMonoid)
      val expected = o.foldLeft("")((b, a) => a ++ b)
      actual == expected
    }

  //Fold tests for Tree

  import chapter3.{Tree, Branch, NilNode, Leaf}
  val f:(Int => Int) = b => b
  val g:((Int,Int) => Int) = (b1: Int, b2: Int) => math.max(b1, b2)
  val h:Int = Int.MinValue

  property("Test foldLeft maximum on empty tree ") = {
    val t = Branch(NilNode, NilNode)
    val actual = FoldableInstances.tree.foldLeft(t)(h)(g)
    actual == Int.MinValue
  }

  property("Test foldLeft maximum with Tree of 2 leaves of Ints") = {
    val t = Branch(Leaf(1200), Leaf(2))
    val actual = FoldableInstances.tree.foldLeft(t)(h)(g)
    actual == 1200
  }

  property("Test foldRight maximum on empty tree ") = {
    val t = Branch(NilNode, NilNode)
    val actual = FoldableInstances.tree.foldRight(t)(h)(g)
    actual == Int.MinValue
  }

  property("Test foldRight maximum with Tree of 2 leaves of Ints") = {
    val t = Branch(Leaf(1200), Leaf(2))
    val actual = FoldableInstances.tree.foldRight(t)(h)(g)
    actual == 1200
  }

  property("Test foldMap stringMonoid on empty tree ") = {
    val t = Branch(NilNode, NilNode)
    val actual = FoldableInstances.tree.foldMap(t)(_.toString)(stringMonoid)
    actual == ""
  }

  property("Test foldMap stringMonoid with Tree of 2 leaves of Ints") = {
    val t = Branch(Leaf(1200), Leaf(2))
    val actual = FoldableInstances.tree.foldMap(t)(_.toString)(stringMonoid)
    actual == "12002"
  }
}
