package rwalerow.chapter2

import scala.annotation.tailrec

object Exercise1 {
  def fib(n: Int): Int = {
    @tailrec
    def inner(counter: Int, first: Int, second: Int): Int = {
      if(counter <= 2) second
      else inner(counter - 1, second, first + second)
    }

    if(n <= 1) return 0
    else inner(n, 0, 1)
  }
}
