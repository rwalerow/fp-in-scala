package rwalerow.chapter4

import org.scalatest.{Matchers, WordSpec, fixture}
import rwalerow.chapter4.Option.traverse

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
    "map2" should {
      "work fine for both some" in {
        Option.map2(Some(2), Some(3))(_ + _) shouldBe Some(5)
      }
      "return none if first is none" in {
        Option.map2(None:Option[Int], Some(11))(_ + _) shouldBe None
      }
      "return none if second is none" in {
        Option.map2(Some(10), None:Option[Int])(_ + _) shouldBe None
      }
    }
    "sequence" should {
      "transform proper list" in  {
        Option.sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1,2,3))
      }
      "return None when None was at the end of list" in {
        Option.sequence(List(Some(1), Some(2), None)) shouldBe None
      }
      "return None when None was at the start of list" in {
        Option.sequence(List(None, Some(1), Some(2))) shouldBe None
      }
      "return None when None was in the middle of list" in {
        Option.sequence(List(Some(1), None, Some(2))) shouldBe None
      }
    }
    "traverse" should {
      "transform int to strings" in {
        traverse(List(1,2,3,4))(x =>  Some(x.toString())) shouldBe Some(List("1", "2", "3", "4"))
      }
      "traverse should handle None case" in {
        traverse(List(1,2,3,4)) {
          case 3 => None
          case x => Some(x)
        } shouldBe None
      }
    }
  }
}
