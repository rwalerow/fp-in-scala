package rwalerow.chapter4

import org.scalatest.{Matchers, WordSpec}

class EitherSpec extends WordSpec with Matchers {

  "Either" should {
    "map" should {
      "work ok for right" in {
        val a = Right(11)
        a map (_ + 10) shouldBe Right(21)
      }

      "propagate left" in {
        val a: Either[String, Int] = Left("Some errors")
        a map (_ + 10) shouldBe a
      }
    }
    "flatMap" should {
      "always flatmap left to left" in {
        Left("Error") flatMap (_ => Right(110)) shouldBe Left("Error")
      }
      "always flatmap to the same left" in {
        Left("1") flatMap (_ => Left("2")) shouldBe Left("1")
      }
    }
    "orElse" should {
      "have no effect on right" in {
        Right(10) orElse Right(100) shouldBe Right(10)
      }
      "take orElse on left" in {
        Left("error") orElse Right(100) shouldBe Right(100)
      }
    }
    "map2" should {
      "not work on first being left" in {
        Left("error").map2(Right(10))((a: Int, b: Int) => a + b) shouldBe Left("error")
      }
      "not work on second being left" in {
        Right(10).map2(Left("error"))((a: Int, b: Int) => a + b) shouldBe Left("error")
      }
      "perform operation when both are valid" in {
        Right(10).map2(Right(15))(_ + _) shouldBe Right(25)
      }
      "return first left when both are invalid" in {
        Left("1").map2(Left("2"))((a: Int, b: Int) => a + b) shouldBe Left("1")
      }
    }
  }

}
