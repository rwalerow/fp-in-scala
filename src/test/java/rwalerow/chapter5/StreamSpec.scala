package rwalerow.chapter5

import org.scalatest.{Matchers, WordSpec}

class StreamSpec extends WordSpec with Matchers {

  "Stream" should {
    "toList" should {
      "work for empty stream" in {
        val a = Stream(1, 2, 3, 4, 5, 6)
        a.toList shouldBe List(1, 2, 3, 4, 5, 6)
      }
    }
    "take" should {
      "take only requerd number of elements" in {
        val a = Stream(1, 2, 3, 4, 5)
        val taken = a.take(3).toList
        taken.size shouldBe 3
        taken.last shouldBe 3
      }
      "not call untaken elements" in {
        val a = Cons(() => 2,
          () => Cons(() => { throw new Exception("sad"); 2},
            () => Cons(() => 2,
              () => Cons(() => 2,
                () => Cons(() => 2,
                  () => Empty)))))
        a.take(1).toList shouldBe List(2)
      }
    }
    "takeWhile" should {
      "take only even elements" in {
        val a = Stream(2,4,6,8,11,12)
        a.takeWhile(_ % 2 == 0).toList shouldBe List(2,4,6,8)
      }
    }
    "for all" should {
      "work for all" in {
        Stream(2,4,5,6,7).forAll(_ > 0) should be (true)
      }
      "detect false case" in {
        Stream(2,3,4,5,6).forAll(_ != 6) should be (false)
      }
    }
  }

}
