package rwalerow.chapter4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](default: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

case object None extends Option[Nothing] {
  override def map[B](f: (Nothing) => B): Option[B] = None
  override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None
  override def getOrElse[B >: Nothing](default: => B): B = default
  override def orElse[B >: Nothing](default: => Option[B]): Option[B] = default
  override def filter(f: (Nothing) => Boolean): Option[Nothing] = None
}

case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: (A) => B): Option[B] = Some(f(get))
  override def flatMap[B](f: (A) => Option[B]): Option[B] = f(get)
  override def getOrElse[B >: A](default: => B): B = get
  override def orElse[B >: A](default: => Option[B]): Option[B] = this
  override def filter(f: (A) => Boolean): Option[A] = if(f(get)) this else None
}

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
  def insuranceRateQuote(age: Int, numberOfSpeedTickets: Int): Double = ???

  def parseInsuranceRateQuote(age: String, numberOfSeedingTickets: String) = {
    val optAge = Try(age.toInt)
    val optTickets = Try(numberOfSeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] = try Some(a) catch { case e: Exception => None }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    aval <- a
    bval <- b
  } yield f(aval, bval)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight(Some(List()): Option[List[A]]){
    case (None, _) | (_, None) => None
    case (elementO, accO) => for {
      acc <- accO
      element <- elementO
    } yield element :: acc
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight(Some(List()): Option[List[B]]) { (e, acc) => (f(e), acc) match {
        case (None, _) | (_, None) => None
        case (elementO, accO) => map2(elementO, accO)(_ :: _)
      }
    }
  }
}
