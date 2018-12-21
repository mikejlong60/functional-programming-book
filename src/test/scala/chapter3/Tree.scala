package chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  import scala.math.max

  def map[A, B](tree: Tree[A])(f: A => B):Tree[B] = tree match {
    case Branch(l: Branch[A], r: Branch[A])  => Branch(map(l)(f), map(r)(f))
    case Branch(null, r: Branch[A]) => Branch(null, map(r)(f))
    case Branch(l: Branch[A], null) => Branch(map(l)(f), null)
    case Branch( l: Branch[A],  r: Leaf[A]) => Branch(map(l)(f), Leaf(f(r.value)))
    case Branch( l: Leaf[A],  r: Branch[A]) => Branch(Leaf(f(l.value)), map(r)(f))
    case Branch(l: Leaf[A], r: Leaf[A]) => Branch(Leaf(f(l.value)), Leaf(f(r.value)))
    case Branch(null, r: Leaf[A]) => Branch(null, Leaf(f(r.value)))
    case Branch(l: Leaf[A], null) => Branch(Leaf(f(l.value)), null)
    case _ => Branch(null, null)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
     case Branch(l: Branch[Int], r: Branch[Int])  => math.max(maximum(l),maximum(r))
     case Branch(null, r: Branch[Int]) => maximum(r)
     case Branch(l: Branch[Int], null) => maximum(l)
     case Branch( l: Branch[Int],  r: Leaf[Int]) => math.max(r.value, maximum(l))
     case Branch( l: Leaf[Int],  r: Branch[Int]) => math.max(l.value, maximum(r))
     case Branch(l: Leaf[Int], r: Leaf[Int]) => math.max(l.value, r.value)
     case Branch(null, r: Leaf[Int]) => r.value
     case Branch(l: Leaf[Int], null) => l.value
     case _ => Int.MinValue
   }

  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(l: Branch[A], r: Branch[A])  => 2 + size(l) + size(r)
    case Branch(null, r: Branch[A]) => 1 + size(r)
    case Branch(l: Branch[A], null) => 1 + size(l)
    case Branch( l: Branch[A],  r: Leaf[A]) => 2 + size(l)
    case Branch( l: Leaf[A],  r: Branch[A]) => 2 + size(r)
    case Branch(l: Leaf[A], r: Leaf[A]) => 2
    case Branch(null, r: Leaf[A]) => 1
    case Branch(l: Leaf[A], null) => 1
    case _ => 0
  }

  def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B): B = tree match {
    case Branch(left, right) if left != null => left match {
      case Leaf(l) => fold(Branch(null, right), f(l, z))(f)
      case Branch(l, r) => fold(Branch(l, r),z)(f)
    }
    case Branch(left, right) if right != null => right match {
      case Leaf(r) => fold(Branch(left, null), f(r,z))(f)
      case Branch(l, r) => fold(Branch(l, r), z)(f)
    }
    case _ => z
  }
}
