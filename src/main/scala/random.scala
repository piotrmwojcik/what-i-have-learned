import State.Rand

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

/*object SimpleRNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rngv.nextInt
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

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (i, rng2) = f(rng)
    g(i)(rng2)
  }

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def _nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i  =>
    if (i + (n - 1) - (i % n) >= 0) unit(i % n) else nonNegativeLessThan(n)
  }

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => _map(rb)(b => f(a, b)))
}*/

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](g: A => State[S, B]): State[S, B] = State(s => {
    val (i, s2) = run(s)
    g(i).run(s2)
  })

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def both[B](rb: State[S, B]): State[S, (A, B)] = map2(rb)((_, _))

  def set(s: S): State[S, Unit] = State(_ => ((), s))

  def get: State[S, S] = State(s => (s, s))

  def modify(f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State(s => {
    val (i, s2) = s.nextInt
    (i, s2)
  })

  val nonNegativeInt: Rand[Int] = State(s => {
    val (i, r) = s.nextInt
    (if (i < 0) -(i + 1) else i, r)
  })

  val nonNegativeEven: Rand[Int] = nonNegativeInt.map(i => i - i % 2)

  def double: Rand[Double] = State(s => {
    val (i, r) = nonNegativeInt.run(s)
    (i / (Int.MaxValue.toDouble + 1), r)
  })

  def nonNegativeLessThan(n: Int): Rand[Int] = State(s => {
    val (i, rng2) = nonNegativeInt.run(s)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, rng2)
    else nonNegativeLessThan(n).run(s)
  })

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]]
  = fs.foldRight(unit(List[A]()): State[S, List[A]])((f, acc) => f.map2(acc)(_ :: _))

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))


}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val a = inputs.map(i => State.unit[Machine, Unit](()).modify(i match {
      case Coin => (m: Machine) => if (m.candies > 0) Machine(locked = false, m.candies, m.coins + 1) else m
      case Turn => (m: Machine) => if (!m.locked && m.candies > 0) Machine(locked = true, m.candies - 1, m.coins) else m
    }))
    val dupa = a.foldRight(State.unit[Machine, Unit](()))((x, acc) => x.flatMap(_ => acc)).get
    dupa.map(m => (m.candies, m.coins))
  }
}

object random extends App {
  /*println(State.int.map2(State.int)((  _, _)).run(SimpleRNG(123)))
  println(State.int.both(State.int).run(SimpleRNG(123)))
  println(State.int.run(SimpleRNG(123)))
  println(State.unit(5).run(SimpleRNG(123)))
  println(State.nonNegativeInt.run(SimpleRNG(21989)))
  println(State.double.run(SimpleRNG(1223)))
  println(State.ints(20).run(SimpleRNG(26051989)))
  println(State.nonNegativeEven.run(SimpleRNG(123)))*/

  val m = Machine(locked = true, 5, 10)

  val a = State.unit[Machine, Unit](()).modify(m => Machine(m.locked, m.candies + 1, m.coins))
  val b = State.unit[Machine, Unit](()).modify(m => Machine(m.locked, m.candies, m.coins - 1))

  println(a.flatMap(_ => b.flatMap(_ => State.unit[Machine, Unit](()))).run(m))
  println(List(a, b).foldRight(State.unit[Machine, Unit](()))((x, acc) => x.flatMap(_ => acc)).get.map(m => (m.candies, m.coins)).run(m))
  //println(Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(m))

  val ns: Rand[List[Int]] = for {
    x <- State.int
    y <- State.int
    xs <- State.ints(x)
  } yield xs.map(_ % y)
}
