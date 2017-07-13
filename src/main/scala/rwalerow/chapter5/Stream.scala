package rwalerow.chapter5

trait Stream[+A] {
  import Stream._

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

  def mapViaFold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(head, tail) => Some((f(head()), tail()))
    case _ => None
  }

  def takeViaFold(n: Int): Stream[A] = unfold((this, n)) {
    case (_, 0) | (Empty, _) => None
    case (Cons(head, tail), left) => Some((head(), (tail(), left - 1)))
    case _ => None
  }

  def takeWhileViaFold(p: A => Boolean): Stream[A] = unfold(this) {
    case Empty => None
    case Cons(head, _) if !p(head()) => None
    case Cons(head, tail) => Some(head(), tail())
  }

  def zipWithViaFold[B](other: Stream[B]): Stream[(A, B)] = unfold((this, other)) {
    case (Empty, _) | (_, Empty) => None
    case (Cons(thisHead, thisTail), Cons(otherHead, otherTail)) =>
      Some((thisHead(), otherHead()), (thisTail(), otherTail()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(thisHead, thisTail), Cons(otherHead, otherTail)) =>
      Some((Some(thisHead()), Some(otherHead())), (thisTail(), otherTail()))
    case (Empty, Cons(s2head, s2Tail)) => Some((None, Some(s2head())), (this, s2Tail()))
    case (Cons(head, tail), Empty) => Some((Some(head()), None), (tail(), Empty))
    case _ => None
  }

  def startWith[A](s: Stream[A]): Boolean = {
    zipAll(s)
      .filter {
        case (Some(_), Some(_)) => true
        case _ => false
      }.forAll {
        case (h, sh) => h == sh
        case _ => false
      }
  }

  def tails: Stream[Stream[A]] = unfold(this){
    case e@Cons(_, tail) => Some((e, tail()))
    case _ => None
  }

  def tailsViaScanRight: Stream[Stream[A]] = scanRight(Empty: Stream[A])((elem, acc) => Cons(() => elem, () => acc))

  def scanRight[B](default: B)(f: (A, => B) => B): Stream[B] = unfold(this) {
    case e@Cons(head, tails) => Some((e.foldRight(default)(f), tails()))
    case _ => None
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

  def fibs: Stream[Int] = fibsFrom(0, 1)

  def fibsFrom(first: Int, second: Int): Stream[Int] = cons(first, fibsFrom(second, first + second))

  def empty[A]: Stream[A] = Empty

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

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
