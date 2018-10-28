package fpinscala.introduction.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Exercise 3.25
  // Implement function `size` that counts the number of nodes (leaves
  // and branches) in a Tree
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }

  // Exercise 3.26
  // Implement function `maximum` that returns max element in a Tree[Int]
  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(v) => v
  }

  // Exercise 3.27
  // Implement function `depth` that returns max path length from the
  // root to any leaf
  def depth[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => 1 + (depth(l) max depth(r))
    case Leaf(_) => 0
  }

  // Exercise 3.28
  // Implement function `map`
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v) => Leaf(f(v))
  }

  // Exercise 3.29
  // Generalize `size`, `maximum`, `depth` and `map`, writing a function
  // `fold` that abstracts over their similarities. Reimplement them in
  // terms of more general function
  def fold[A, B](t: Tree[A])(combine: (B, B) => B, f: A => B): B = t match {
    case Branch(l, r) => combine(fold(l)(combine, f), fold(r)(combine, f))
    case Leaf(v) => f(v)
  }
  def sizeViaFold[A](t: Tree[A]): Int = fold[A, Int](t)(1 + _ + _, _ => 1)
  def maxViaFold(t: Tree[Int]): Int = fold[Int, Int](t)(_ max _, identity)
  def depthViaFold[A](t: Tree[A]): Int = fold[A, Int](t)(1 + _ max _, _ => 0)
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(Branch(_, _), v => Leaf(f(v)))
}
