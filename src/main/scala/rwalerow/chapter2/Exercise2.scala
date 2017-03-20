package rwalerow.chapter2

object Exercise2 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def inner(n: Int, f: A, s: A): Boolean = {
      if (!ordered(f, s)) false
      else if( n == as.length - 1) true
      else inner(n + 1, s, as(n + 1))
    }

    if(as.length <= 1) true
    else inner(1, as(0), as(1))
  }
}
