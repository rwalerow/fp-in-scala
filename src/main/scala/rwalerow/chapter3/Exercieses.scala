package rwalerow.chapter3

import rwalerow.chapter3.List.{foldRight, foldRight2, reverse}

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

  // 3.16
  def addOne(as: List[Int]): List[Int] =
    foldRight2(as, Nil: List[Int])((ele, acc) => Cons(ele + 1, acc))

  // 3.17
  def stringifyList(as: List[Double]): List[String] =
    foldRight2(as, Nil:List[String])((ele, acc) => Cons(ele.toString, acc))

  // 3.22
  def addElements(f: List[Int], s: List[Int]): List[Int] = (f, s) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(fh, ft), Cons(sh, st)) => Cons(fh + sh, addElements(ft, st))
  }

  def addElements2(f: List[Int], s: List[Int]): List[Int] = {
    def inner(first: List[Int], second: List[Int])(acc: List[Int]): List[Int] = (first, second) match {
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (Cons(fh, ft), Cons(sh, st)) => inner(ft, st)(Cons(fh + sh, acc))
    }

    reverse(inner(f, s)(Nil))
  }
}
