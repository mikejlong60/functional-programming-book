package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class TreeTest extends PropSpec with PropertyChecks with Matchers {

  property("Test size with Tree of 2 leaves of Ints") {
    val t = Branch(Leaf(1), Leaf(2))
    val actual = Tree.size(t)
    actual should be (2)
  }

   property("Test size with Tree of right 1 leaf") {
     val t = Branch(Leaf(1), null)
     val actual = Tree.size(t)
     actual should be (1)
   }

   property("Test size with Tree of left 1 leaf and null") {
     val t = Branch(null, Leaf(2))
     val actual = Tree.size(t)
     actual should be (1)
   }

   property("Test size with Tree of 2 Branches woth 2 leaves on each one") {
     val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
     val actual = Tree.size(t)
     actual should be (6)
   }

   property("Test left leaning tree with Branch at top only on left") {
     val t = Branch(Branch(Leaf(1), Leaf(2)), null)
     val actual = Tree.size(t)
     actual should be (3)
   }

   property("Test size with Tree of 2 nulls") {
     val t = Branch(null, null)
     val actual = Tree.size(t)
     actual should be (0)
   }
   property("Test size with Tree of 3 even layers") {
     val l3ll = Branch(Leaf(1), Leaf(2))
     val l3lr = Branch(Leaf(3), Leaf(4))
     val r3ll = Branch(Leaf(1), Leaf(2))
     val r3lr = Branch(Leaf(3), Leaf(4))
     val t = Branch(Branch(l3ll, l3lr), Branch(r3ll, r3lr))
     val actual = Tree.size(t)
     actual should be (14)
   }

////  property("Test fold with Tree of 2 leaves of Ints") {
//    val t = Branch(Leaf(1), Leaf(2))
//    val actual = Tree.fold(t, 0)((x, a) => a + 1)
//    actual should be (2)
//  /}

//  property("Test fold with left branch of 2 leaves and right branch of leaf of Ints") {
//    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
//    val actual = Tree.fold(t, 0)((x, a) => a + 1)
//    actual should be (3)
//  }

//  property("Test fold with left branch of 2 leaves and right branch of 2 leaves of Ints") {
//    val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
//    val actual = Tree.fold(t, 0)((x, a) => a + 1)
//    actual should be (4)
//  }


}
