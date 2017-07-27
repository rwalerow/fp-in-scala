package rwalerow.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}


object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (next, newRng) if next == Int.MinValue => (Int.MaxValue, newRng)
    case (next, newRng) if next < 0 => (-next, newRng)
    case state => state
  }

  def double(rng: RNG): (Double, RNG) = {
    val (next, newRng) = nonNegativeInt(rng)
    (next.toDouble/Int.MaxValue, newRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, newRng1) = rng.nextInt
    val (d, newRng2) = double(newRng1)
    ((i, d), newRng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = intDouble(rng) match {
    case ((intVal, doubleValue), r) => ((doubleValue, intVal), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (double1, rng1) = double(rng)
    val (double2, rng2) = double(rng1)
    val (double3, rng3) = double(rng2)
    ((double1, double2, double3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (Nil, rng)
    case x => {
      val (newVal, newRng) = rng.nextInt
      val (list, returnRng) = ints(count - 1)(newRng)
      (newVal :: list, returnRng)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap: Rand[Double] =
    map(nonNegativeEven)(i => i.toDouble/Int.MaxValue)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rnga) = ra(rng)
      val (b, rngb) = rb(rnga)
      (f(a, b), rngb)
    }
}

