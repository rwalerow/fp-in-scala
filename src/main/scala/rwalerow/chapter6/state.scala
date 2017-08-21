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

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List()): Rand[List[A]])((acc, nextRand) => map2(acc, nextRand)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =  rng =>
    f(rng) match {
      case (valA, rng2) => g(valA)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if( i + (n-1) - mod >= 0) (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State(
    state => run(state) match {
      case (a, s) => (f(a), s)
    }
  )

  def map2[B, C](sc: State[S, B])(f: (A, B) => C): State[S, C] = State(
    state => run(state) match {
      case (va, sa) => sc.run(sa) match {
        case (vb, sb) => (f(va, vb), sb)
      }
    }
  )

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    state => run(state) match {
      case (va, sa) => f(va).run(sa)
    }
  )
}

object State {
  type Rand[A] = State[RNG, A]

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = ???
}