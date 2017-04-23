package rwalerow.chapter4

import org.scalatest.{Matchers, WordSpec, fixture}

class OptionSpec extends WordSpec with Matchers {

  "Option" should {
    "map" should {
      "map from None to None" in {
        val o: Option[Int] = None
        o map (_ + 1) shouldBe None
      }

      "map add 1 to int inside" in {
        Some(1) map (_ + 1) shouldBe Some(2)
      }

      "map int to String" in {
        Some(1) map (_.toString) shouldBe Some("1")
      }
    }

    "flatMap" should {
      "flatMap none to none" in {
        val o: Option[Int] = None
          o flatMap (x => Some(x.toString)) shouldBe None
      }

      "flatMap fine with identyty" in {
        Some(1) flatMap (Some(_)) shouldBe Some(1)
      }
    }

    "getOrElse" should {
      "take default in case of none" in {
        None.getOrElse(11) shouldBe 11
      }
      "take value in case of Some" in {
        Some(11) getOrElse(100) shouldBe 11
      }
    }

    "orElse" should {
      "take default in case of None" in {
        None.orElse(Some(11)) shouldBe Some(11)
      }

      "take original value in case of some" in {
        Some(11).orElse(Some(100)) shouldBe Some(11)
      }
    }

    "filter" should {
      "do nothing on None" in {
        None.filter((x: Int) => x > 10) shouldBe None
      }

      "filter out when false" in {
        Some(11) filter (_ < 10) shouldBe None
      }

      "leave value when true" in {
        Some(11) filter (_ > 10) shouldBe Some(11)
      }

    }
  }
}
