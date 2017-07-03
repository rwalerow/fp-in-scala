package rwalerow.chapter5

import rwalerow.chapter5.Stream.{cons, empty}

trait Stream[+A] {
  def toList: List[A]
  def take(n: Int): Stream[A]
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def map[B](f: A => B): Stream[B] = this match {
    case Empty => Empty
    case Cons(h, t) => Cons(() => f(h()), () => t() map f)
  }

  def filter(predicate: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if(predicate(h())) Cons(() => h(), () => t().filter(predicate)) else t().filter(predicate)
  }

  def append[B >: A](second: Stream[B]): Stream[B] = this match {
    case Empty => second
    case Cons(h, t) => Cons(h, () => t().append(second))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = this match {
    case Empty => Empty
    case Cons(h, t) => foldRight(Empty: Stream[B])(f(_).append(_))
  }
}

case object Empty extends Stream[Nothing] {
  override def toList: List[Nothing] = Nil
  override def take(n: Int): Stream[Nothing] = Empty
}

case class Cons[+A](h: () => A,
                    t: () => Stream[A]) extends Stream[A] {
  override def toList: List[A] =
    h() :: t().toList
  override def take(n: Int): Stream[A] =
    if(n >= 1) Cons(h, () => t().take(n - 1)) else Empty
}

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val const: Stream[A] = cons(a, const)
    const
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def empty[A]: Stream[A] = Empty

  /**
    *
    * I do believe that this method(which is listed in the book)
    * isn't working as expected.
    * I do believe it evaluate all elements before creating a stream
    * Example to support my claim:
    * Stream(1, { throw new Exception("Should not be called"); 2 })
    *
    */
  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
