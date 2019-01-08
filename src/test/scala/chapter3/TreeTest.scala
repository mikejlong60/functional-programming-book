package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}


//TODO Make a Scalacheck Arbitrary version for my Tree here.  Then most of these tests would be unnecessary.
class TreeTest extends PropSpec with PropertyChecks with Matchers {

  //Map tests --- Cannot compare directly because I have no equals method on my Tree
  property("Test map with Tree of 2 leaves of Ints") {
    val t = Branch(Leaf(1), Leaf(2))
    val actual = Tree.map(t)(x => x * 12)
    Tree.size(actual) should be  (3)
    Tree.maximum(actual) should be (24)
  }

  property("Test map with Tree of right 1 leaf") {
    val t = Branch(Leaf(1), NilNode)
    val actual = Tree.map(t)(x => x * 12)
    Tree.size(actual) should be  (2)
    Tree.maximum(actual) should be (12)
  }

  property("Test map with Tree of left 1 leaf and null") {
    val t = Branch(NilNode, Leaf(2))
    val actual = Tree.map(t)(x => x * 12)
    Tree.size(actual) should be  (2)
    Tree.maximum(actual) should be (24)
  }

  property("Test map with Tree of 2 Branches with 2 leaves on each one") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val actual = Tree.map(t)(x => x * 12)
    Tree.size(actual) should be  (7)
    Tree.maximum(actual) should be (48)
  }

  property("Test map with Tree of 3 even layers") {
    val l3ll = Branch(Leaf(1), Leaf(2))
    val l3lr = Branch(Leaf(3), Leaf(4))
    val r3ll = Branch(Leaf(1), Leaf(2))
    val r3lr = Branch(Leaf(3), Leaf(4))
    val t = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val actual = Tree.map(t)(x => x * 12)
    Tree.size(actual) should be  (15)
    Tree.maximum(actual) should be (48)
  }

  property("Test map on left leaning tree with Branch at top only on left") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), NilNode)
    val actual = Tree.map(t)(x => x * 12)
    Tree.size(actual) should be  (4)
    Tree.maximum(actual) should be (24)
  }

  property("Test map with Tree of 2 nulls") {
    val t: Tree[Int] = Branch(NilNode, NilNode)
    val actual = Tree.map(t)(x => x * 12)
    Tree.size(actual) should be  (1)
    Tree.maximum(actual) should be (Int.MinValue)
  }

  property("Test map big Tree") {
    val l3ll = Branch(Leaf(1), Leaf(2))
    val l3lr = Branch(Leaf(3), Leaf(4))
    val r3ll = Branch(Leaf(1), Leaf(2))
    val r3lr = Branch(Leaf(3), Leaf(4))
    val l = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val r = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val t = Branch(Branch(l, r), Branch(l, r))
    val actual = Tree.map(t)(x => x.toFloat / 2)
    Tree.size(actual) should be  (63)
  }

  property("Test map equivalence") {
    val l3ll = Branch(Leaf(1), Leaf(2))
    val l3lr = Branch(Leaf(3), Leaf(4))
    val r3ll = Branch(Leaf(1), Leaf(2))
    val r3lr = Branch(Leaf(3), Leaf(4))
    val l = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val r = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val t = Branch(Branch(l, r), Branch(l, r))

    val actual = Tree.map(t)(x => x)
    actual should be (t)
  }

  //Max tests
  property("Test max with Tree of 2 leaves of Ints") {
    val t = Branch(Leaf(1), Leaf(2))
    val actual = Tree.maximum(t)
    actual should be (2)
  }



  property("Test max with Tree of right 1 leaf") {
    val t = Branch(Leaf(1), NilNode)
    val actual = Tree.maximum(t)
    actual should be (1)
  }

  property("Test max with Tree of left 1 leaf and null") {
    val t = Branch(NilNode, Leaf(2))
    val actual = Tree.maximum(t)
    actual should be (2)
  }

  property("Test max with Tree of 2 Branches woth 2 leaves on each one") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val actual = Tree.maximum(t)
    actual should be (4)
  }

  property("Test max on left leaning tree with Branch at top only on left") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), NilNode)
    val actual = Tree.maximum(t)
    actual should be (2)
  }

  property("Test max with Tree of 2 nulls") {
    val t = Branch(NilNode, NilNode)
    val actual = Tree.maximum(t)
    actual should be (Int.MinValue)
  }

  property("Test max with Tree of 3 even layers") {
    val l3ll = Branch(Leaf(1), Leaf(2))
    val l3lr = Branch(Leaf(3), Leaf(4))
    val r3ll = Branch(Leaf(1), Leaf(2))
    val r3lr = Branch(Leaf(3), Leaf(4))
    val t = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val actual = Tree.maximum(t)
    actual should be (4)
  }

  property("Test max big Tree") {
    val l3ll = Branch(Leaf(1), Leaf(2))
    val l3lr = Branch(Leaf(3), Leaf(4))
    val r3ll = Branch(Leaf(1), Leaf(2))
    val r3lr = Branch(Leaf(3), Leaf(4))
    val l = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val r = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val t = Branch(Branch(l, r), Branch(l, r))
    val actual = Tree.maximum(t)
    actual should be (4)
  }


  //Size tests
  property("Test size with Tree of 2 leaves of Ints") {
    val t = Branch(Leaf(1), Leaf(2))
    val actual = Tree.size(t)
    actual should be (3)
  }

  property("Test size with Tree of right 1 leaf") {
    val t = Branch(Leaf(1), NilNode)
    val actual = Tree.size(t)
    actual should be (2)
  }

  property("Test size with Tree of left 1 leaf and null") {
    val t = Branch(NilNode, Leaf(2))
    val actual = Tree.size(t)
    actual should be (2)
  }

   property("Test size with Tree of 2 Branches with 2 leaves on each one") {
     val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
     val actual = Tree.size(t)
     actual should be (7)
   }

  property("Test size with Tree of 3 even layers") {
     val l3ll = Branch(Leaf(1), Leaf(2))
     val l3lr = Branch(Leaf(3), Leaf(4))
     val r3ll = Branch(Leaf(1), Leaf(2))
     val r3lr = Branch(Leaf(3), Leaf(4))
     val t = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
     val actual = Tree.size(t)
     actual should be (15)
   }

  property("Test size on left leaning tree with Branch at top only on left") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), NilNode)
    val actual = Tree.size(t)
    actual should be (4)
  }

  property("Test size with Tree of 2 nulls") {
    val t = Branch(NilNode, NilNode)
    val actual = Tree.size(t)
    actual should be (1)
  }

  property("Test size big Tree") {
     val l3ll = Branch(Leaf(1), Leaf(2))
     val l3lr = Branch(Leaf(3), Leaf(4))
     val r3ll = Branch(Leaf(1), Leaf(2))
     val r3lr = Branch(Leaf(3), Leaf(4))
     val l = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
     val r = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
     val t = Branch(Branch(l, r), Branch(l, r))
     val actual = Tree.size(t)
     actual should be (63)
   }

/// Depth tests
  property("Test depth with Tree of 2 leaves of Ints") {
    val t = Branch(Leaf(1), Leaf(2))
    val actual = Tree.depth(t)
    actual should be (1)
  }


  property("Test depth with Tree of right 1 leaf") {
    val t = Branch(Leaf(1), NilNode)
    val actual = Tree.depth(t)
    actual should be (1)
  }

  property("Test depth with Tree of left 1 leaf and null") {
    val t = Branch(NilNode, Leaf(2))
    val actual = Tree.depth(t)
    actual should be (1)
  }

  property("Test depth with Tree of 2 Branches woth 2 leaves on each one") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val actual = Tree.depth(t)
    actual should be (2)
  }

  property("Test depth on left leaning tree with Branch at top only on left") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), NilNode)
    val actual = Tree.depth(t)
    actual should be (2)
  }

  property("Test depth with Tree of 2 nulls") {
    val t = Branch(NilNode, NilNode)
    val actual = Tree.depth(t)
    actual should be (1)
  }

  property("Test depth with Tree of 3 even layers") {
    val l3ll = Branch(Leaf(1), Leaf(2))
    val l3lr = Branch(Leaf(3), Leaf(4))
    val r3ll = Branch(Leaf(1), Leaf(2))
    val r3lr = Branch(Leaf(3), Leaf(4))
    val t = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val actual = Tree.depth(t)
    actual should be (3)
  }

  property("Test depth big Tree") {
    val l3ll = Branch(Leaf(1), Leaf(2))
    val l3lr = Branch(Leaf(3), Leaf(4))
    val r3ll = Branch(Leaf(1), Leaf(2))
    val r3lr = Branch(Leaf(3), Leaf(4))
    val l = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val r = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val t = Branch(Branch(l, r), Branch(l, r))
    val actual = Tree.depth(t)
    actual should be (5)
  }


  //fold tests
  val f:(Int => Int) = b => b
  val g:((Int,Int) => Int) = (b1, b2) => math.max(b1, b2)
  val h: (() => Int) = () => Int.MinValue
  property("Test fold maximum on empty tree ") {
    val t = Branch(NilNode, NilNode)

    val actual = Tree.fold(t)(f)(g)(h)
    actual should be (Int.MinValue)
  }

  property("Test fold maximum with Tree of 2 leaves of Ints") {
    val t = Branch(Leaf(1200), Leaf(2))

    val actual = Tree.fold(t)(f)(g)(h)
    actual should be (1200)
  }


  property("Test fold with Tree of right 1 leaf") {
    val t = Branch(Leaf(1), NilNode)
    val actual = Tree.fold(t)(f)(g)(h)
    actual should be (1)
  }


  property("Test fold with Tree of left 1 leaf and null") {
    val t = Branch(NilNode, Leaf(2))
    val actual = Tree.fold(t)(f)(g)(h)
    actual should be (2)
  }

  property("Test fold with Tree of 2 Branches with 2 leaves on each one") {
    val t = Branch(Branch(Leaf(10000), Leaf(200)), Branch(Leaf(3), Leaf(4)))
    val actual = Tree.fold(t)(f)(g)(h)
    actual should be (10000)
  }


  property("Test fold on left leaning tree with Branch at top only on left") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), NilNode)
    val actual = Tree.fold(t)(f)(g)(h)
    actual should be (2)
  }

  property("Test fold with Tree of 2 nulls") {
    val t = Branch(NilNode, NilNode)
    val actual = Tree.fold(t)(f)(g)(h)
    actual should be (Int.MinValue)
  }

  property("Test fold with Tree of 3 even layers") {
    val l3ll = Branch(Leaf(1), Leaf(2))
    val l3lr = Branch(Leaf(3), Leaf(4))
    val r3ll = Branch(Leaf(1000), Leaf(2))
    val r3lr = Branch(Leaf(3), Leaf(4))
    val t = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val actual = Tree.fold(t)(f)(g)(h)
    actual should be (1000)
  }

  property("Test fold big Tree") {
    val l3ll = Branch(Leaf(-1000), Leaf(-2))
    val l3lr = Branch(Leaf(3), Leaf(-4))
    val r3ll = Branch(Leaf(1), Leaf(-2))
    val r3lr = Branch(Leaf(3), Leaf(4))
    val l = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val r = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val t = Branch(Branch(l, r), Branch(l, r))
    val actual = Tree.fold(t)(f)(g)(h)
    actual should be (4)
  }

  property("Test fold equivalence") {
    val l3ll = Branch(Leaf(-1000), Leaf(-2))
    val l3lr = Branch(Leaf(3), Leaf(-4))
    val r3ll = Branch(Leaf(1), Leaf(-2))
    val r3lr = Branch(Leaf(3), Leaf(4))
    val l = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val r = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
    val t = Branch(Branch(l, r), Branch(l, r))
    val actual = Tree.fold(t)(Leaf(_): Tree[Int])((l, r) => Branch(l, r): Tree[Int])(() => NilNode: Tree[Int])
    actual should be (t)
  }
}

