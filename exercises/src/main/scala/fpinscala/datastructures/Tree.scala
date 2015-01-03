package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  def max(tree: Tree[Int]): Int = tree match {
    case Branch(left, right) => max(left).max(max(right))
    case Leaf(value) => value
  }
}