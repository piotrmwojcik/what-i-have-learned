
trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r2) = rng.nextInt
    val (d, r3) = double(r2)
    ((i, d), r3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r2) = intDouble(rng)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r1) = rng.nextInt
    val (d2, r2) = r1.nextInt
    val (d3, r3) = r2.nextInt
    ((d, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (count == 0)
        (xs, r)
      else {
        val (x, rs) = r.nextInt
        go(count - 1, rs, x :: xs)
      }
    }

    go(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def _double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rb1) = ra(rng)
    val (b, rb2) = rb(rb1)
    (f(a, b), rb2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def _randIntDouble: Rand[(Int, Double)] = both(int, _double)

  def _randDoubleInt: Rand[(Double, Int)] = both(_double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)
}

object random extends App {
  println(SimpleRNG.int(SimpleRNG(123)))
  println(SimpleRNG.unit(5)(SimpleRNG(123)))
  println(SimpleRNG.nonNegativeInt(SimpleRNG(21989))._1)
  println(SimpleRNG.double(SimpleRNG(1223))._1)
  println(SimpleRNG.ints(20)(SimpleRNG(26051989)))
  println(SimpleRNG.nonNegativeEven(SimpleRNG(123)))
  println(SimpleRNG._double(SimpleRNG(26051989)))
  println(SimpleRNG._ints(20)(SimpleRNG(26051989)))
}
