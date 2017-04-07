package rwalerow.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A,B](as:List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // 3.11
  def sumLeft(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  def productLeft(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  // 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((acc, el) => Cons(el, acc))

  // 3.13
  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a: A, b: B) => f(b,a))

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B) =
    foldLeft(reverse(as), z)((b:B, a: A) => f(a, b))

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  // 3.14
  def appendFold[A](as: List[A], app: List[A]): List[A] =
    foldRight2(as, app)((elem, acc) => Cons(elem, acc))

  // 3.15
  def concatenate[A](as: List[A]*): List[A] =
    foldRight2(List(as: _*), Nil:List[A])((el, acc) => appendFold(el, acc))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight2(as, Nil:List[B])((el, acc) => Cons(f(el), acc))
}
