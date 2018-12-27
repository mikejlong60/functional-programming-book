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

  def depth(tree: Tree[Int]): Int = tree match {
    case Branch(l: Branch[Int], r: Branch[Int])  => 1 + math.max(depth(l),depth(r))
    case Branch(null, r: Branch[Int]) => 1 + depth(r)
    case Branch(l: Branch[Int], null) => 1 + depth(l)
    case Branch( l: Branch[Int],  r: Leaf[Int]) => 1 + depth(l)
    case Branch( l: Leaf[Int],  r: Branch[Int]) => 1 + depth(r)
    case Branch(l: Leaf[Int], r: Leaf[Int]) => 1
    case Branch(null, r: Leaf[Int]) => 1
    case Branch(l: Leaf[Int], null) => 1
    case _ => 0
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

  def fold(tree: Tree[Int], z: Int)(g: (Int, Int) => Int): Int = tree match {
    case Branch(l: Branch[Int], r: Branch[Int])  => g(fold(l, z)(g), fold(r, z)(g))
    case Branch(null, r: Branch[Int]) => g(z, fold(r, z)(g))
    case Branch(l: Branch[Int], null) => g(z, fold(l, z)(g))
    case Branch( l: Branch[Int],  r: Leaf[Int]) => g(fold(l, z)(g), r.value)
    case Branch( l: Leaf[Int],  r: Branch[Int]) => g(fold(r, z)(g), l.value)
    case Branch(l: Leaf[Int], r: Leaf[Int]) => g(l.value, r.value)
    case Branch(null, r: Leaf[Int]) => g(r.value, z)
    case Branch(l: Leaf[Int], null) => g(l.value, z)
    case _ => z
  }
}
