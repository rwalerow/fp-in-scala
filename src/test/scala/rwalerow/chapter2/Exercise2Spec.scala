package rwalerow.chapter2

import org.scalatest.{Matchers, WordSpec}

class Exercise2Spec extends WordSpec with Matchers {

  "IsSorted" should {
    "count (1,2,3,4,5,6) as sorted" in {
      val input = Array(1,2,3,4,5,6)
      val f = (f: Int, s: Int) => f < s
      Exercise2.isSorted(input, f) shouldBe true
    }

    "count (1,2,3,4,5,7,6) as unsorted" in {
      val input = Array(1,2,3,4,5,7,6)
      val f = (f: Int, s: Int) => f < s
      Exercise2.isSorted(input, f) shouldBe false
    }
  }

}
