package rwalerow.chapter3

import rwalerow.chapter3.List.foldRight

object Exercieses {

  // 3.2
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // 3.3
  def setHead[A](v: A, list: List[A]): List[A] = list match {
    case Nil => Cons(v, Nil)
    case Cons(x, xs) => Cons(v, xs)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case _ if n <= 0 => l
    case Cons(x, xs) => drop(xs, n - 1)
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  // 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, x) => x + 1)
}
