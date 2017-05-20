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

  "Either opts" should {
    "sequence" should {

      "work ok for all ok" in {
        val a = List(Right(1), Right(2), Right(3))
        Either.sequence(a) shouldBe Right(List(1,2,3))
      }

      "crash on first left" in {
        val a = List(Right(1), Left("1"), Left("2"))
        Either.sequence(a) shouldBe Left("1")
      }
    }

    "traverse" should {
      "work for all rights" in {
        val a = List(Right(1), Right(2), Right(3))
        Either.traverse(a)(e => Right(e + 10)) shouldBe Right(List(11, 12, 13))
      }
      "fail for left in list" in {
        val a = List(Right(1), Left("1"), Right(2))
        Either.traverse(a)(e => Right(e + 10)) shouldBe Left("1")
      }
      "fail for left in function" in {
        val a = List(Right(1), Right(2), Right(3))
        Either.traverse(a)(e => if(e == 2) Left("2") else Right(e + 10)) shouldBe Left("2")
      }
    }
  }

}
