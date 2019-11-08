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

}
