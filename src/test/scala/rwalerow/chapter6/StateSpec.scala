package rwalerow.chapter6

import org.scalatest.{Matchers, WordSpec}
import rwalerow.chapter6.RNG.{Rand, SimpleRNG}

class StateSpec extends WordSpec with Matchers {

  "Rng" should {
    "map2 properly" in {
      val first: Rand[Int] = rng => (10, rng)
      val second: Rand[String] = rng => ("15", rng)

      val r = SimpleRNG(100)

      RNG.map2(first, second)(_.toString + _)(r) shouldBe ("1015", r)
    }
  }

}
