package rwalerow.chapter4

trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}
case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: (Nothing) => B): Either[E, B] = this
  override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]): Either[EE, B] = this
  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
}
case class Right[+A](value: A) extends Either[Nothing, A]{
  override def map[B](f: (A) => B): Either[Nothing, B] = Right(f(value))
  override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] = f(value)
  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this
  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b match {
    case error: Left[EE] => error
    case Right(v2) => Right(f(value, v2))
  }
}

object Either {

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldLeft(Right(List()): Either[E, List[A]]){
      case (Left(a), _) => Left(a)
      case (rightList, e) => rightList.map2(e)(_ ++ List(_))
    }

  def traverse[E, A, B](es: List[Either[E, A]])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldLeft(Right(List()): Either[E, List[B]]){
      case (Left(a), _) => Left(a)
      case (rightList, e) => rightList.map2(e.flatMap(f))(_ ++ List(_))
    }
}