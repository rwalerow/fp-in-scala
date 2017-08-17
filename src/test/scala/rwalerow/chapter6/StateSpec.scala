package rwalerow.chapter6

import org.scalatest.{Matchers, WordSpec}
import rwalerow.chapter6.RNG._

class StateSpec extends WordSpec with Matchers {

  "Rng" should {
    "map2 properly" in {
      val first: Rand[Int] = rng => (10, rng)
      val second: Rand[String] = rng => ("15", rng)

      val r = SimpleRNG(100)

      RNG.map2(first, second)(_.toString + _)(r) shouldBe ("1015", r)
    }

    "map and mapViaFlatMap should be the same" in {
      val one: Rand[Int] = unit(1)
      val f: Int => Int = _ + 10

      map(one)(f)(SimpleRNG(1)) shouldBe mapViaFlatMap(one)(f)(SimpleRNG(1))
    }

    "map2 and map2ViaFlatMap should be the same" in {
      val one: Rand[Int] = unit(1)
      val ten: Rand[Int] = unit(10)
      val combine:(Int, Int) => Int = _ + _

      map2(one, ten)(combine)(SimpleRNG(1)) shouldBe map2ViaFlatMap(one, ten)(combine)(SimpleRNG(1))
    }
  }

}
