package rwalerow.chapter2

import org.scalatest.{Matchers, WordSpec}

class Exercise1Spec extends WordSpec with Matchers {

  "Fibonacci" should {
    "be 0 for 1" in {
      Exercise1.fib(1) shouldBe 0
    }

    "be 1 for 2" in {
      Exercise1.fib(2) shouldBe 1
    }

    "be 1 for 3" in {
      Exercise1.fib(3) shouldBe 1
    }

    "be 2 for 4" in {
      Exercise1.fib(4) shouldBe 2
    }

    "be 3 for 5" in {
      Exercise1.fib(5) shouldBe 3
    }

    "be 5 for 6" in {
      Exercise1.fib(6) shouldBe 5
    }

    "be 8 for 7" in {
      Exercise1.fib(7) shouldBe 8
    }

    "be 13 for 8" in {
      Exercise1.fib(8) shouldBe 13
    }
  }
}
