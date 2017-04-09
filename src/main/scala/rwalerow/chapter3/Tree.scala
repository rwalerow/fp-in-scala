package rwalerow.chapter3

import java.lang.Math.max

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l,r) => max(depth(l), depth(r)) + 1
  }

  // 3.28
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // 3.29
  def fold[A,B](tree: Tree[A])(transform: A => B, combine: (B, B) => B): B = tree match {
    case Leaf(x) => transform(x)
    case Branch(l, r) => combine(fold(l)(transform, combine), fold(r)(transform, combine))
  }

  def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1, (a: Int, b: Int) => 1 + a + b)
  def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 1, (a: Int, b: Int) => max(a, b) + 1)
  def map2[A,B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)((a: A) => Leaf(f(a)), (a: Tree[B], b: Tree[B]) => Branch(a, b))
}

