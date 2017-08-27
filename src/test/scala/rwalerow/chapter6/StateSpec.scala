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

  "State" should {
    "map" in {
      val a: State[Unit, Int] = State(s => (10, s))
      a.map(_ + 15).run(()) shouldBe (25, ())
    }

    "map2" in {
      val a: State[Unit, Int] = State(s => (10, s))
      val b: State[Unit, Int] = State(s => (33, s))

      a.map2(b)((va, vb) => (va - 1) * (vb - 30)).run() shouldBe (27, ())
    }

    "flatMap" in {
      val a: State[Unit, Int] = State(s => (10, s))
      a.flatMap(valA => State(s => (valA.toString, s))).run(()) shouldBe ("10", ())
    }
  }

  "Machine" should {
    "work for example 1" in {
      val init = Machine(
        locked = true,
        candies = 5,
        coins = 5
      )
      val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)

      State.simulateMachine(inputs).run(init) shouldBe ((9, 1), Machine(
        locked = true, candies = 1, coins = 9
      ))
    }

    "do nothing on no candies" in {
      val init = Machine(
        locked = true,
        candies = 0,
        coins = 5
      )
      val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
      State.simulateMachine(inputs).run(init) shouldBe ((5,0), init)
    }

    "work until candies are finished" in {
      val init = Machine(
        locked = true,
        candies = 1,
        coins = 5
      )
      val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
      State.simulateMachine(inputs).run(init) shouldBe ((6,0), Machine(true, 0, 6))
    }

    "do nothing when coin inserted to unlock" in {
      val init = Machine(
        locked = false,
        candies = 1,
        coins = 5
      )
      State.simulateMachine(List(Coin)).run(init) shouldBe ((5,1), init)
    }

    "do nothing when turned on locked" in {
      val init = Machine(
        locked = true,
        candies = 1,
        coins = 5
      )
      State.simulateMachine(List(Turn)).run(init) shouldBe ((5,1), init)
    }
  }

}
