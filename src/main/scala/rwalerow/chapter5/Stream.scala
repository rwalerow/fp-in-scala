package rwalerow.chapter5

trait Stream[+A] {
  def toList: List[A]
  def take(n: Int): Stream[A]
  def takeWhile(p: A => Boolean): Stream[A]
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)
}

case object Empty extends Stream[Nothing] {
  override def toList: List[Nothing] = Nil
  override def take(n: Int): Stream[Nothing] = Empty
  override def takeWhile(p: (Nothing) => Boolean): Stream[Nothing] = this
}

case class Cons[+A](h: () => A,
                    t: () => Stream[A]) extends Stream[A] {
  override def toList: List[A] =
    h() :: t().toList
  override def take(n: Int): Stream[A] =
    if(n >= 1) Cons(h, () => t().take(n - 1)) else Empty
  override def takeWhile(p: (A) => Boolean): Stream[A] = {
    lazy val head = h()
    if(p(head)) Cons(() => head, () => t().takeWhile(p)) else Empty
  }
}

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

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