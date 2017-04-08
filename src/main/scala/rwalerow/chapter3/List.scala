package rwalerow.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A,B](as:List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((acc, el) => Cons(el, acc))

  // 3.13
  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a: A, b: B) => f(b,a))

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
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

  // 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight2(as, Nil:List[B])((el, acc) => Cons(f(el), acc))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight2(as, Nil: List[A])((el, acc) => if(f(el)) Cons(el, acc) else acc)

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight2(as, Nil: List[B])((el, acc) => appendFold(f(el), acc))

  // 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if(f(a)) List(a) else Nil)

  // 3.23
  def zipWith[A](as: List[A], sec: List[A])(f: (A, A) => A): List[A] = {
    def inner(first: List[A], second: List[A])(acc: List[A]): List[A] = (first, second) match {
      case (Nil, _)|(_, Nil) => acc
      case (Cons(fh, ft), Cons(sh, st)) => inner(ft, st)(Cons(f(fh,sh), acc))
    }
    reverse(inner(as, sec)(Nil:List[A]))
  }

  def any[A](as: List[A])(f: A => Boolean): Boolean =
    foldLeft(as, false)(_ || f(_))

  // 3.24
  def hasSubseqeunce[A](sup: List[A], sub: List[A]): Boolean = {
    def isElementinSubHead(head: A): Boolean = sub match {
      case Nil => false
      case Cons(h, t) => h == head
    }
    def dropIfEqual[A](elem: A)(compList: List[A]): List[A] = compList match {
      case Nil => Nil
      case Cons(h, t) if h == elem => t
      case _ => compList
    }
    def inner(list: List[A])(candidates: List[List[A]]): Boolean = list match {
      case Nil => any(candidates)(_ == Nil)
      case Cons(headE, tailE) => {
        if(isElementinSubHead(headE)) inner(tailE)(Cons(Exercieses.tail(sub), map(candidates)(dropIfEqual(headE))))
        else inner(tailE)(map(candidates)(dropIfEqual(headE)))
      }
    }
    inner(sup)(Nil)
  }
}
