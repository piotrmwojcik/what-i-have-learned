
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
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MaxValue, r: RNG) => nonNegativeInt(r)
    case (n: Int, r: RNG) if (n > 0) => (n, r)
    case (n: Int, r: RNG) => (-n, r)
  }
}

object random extends App {
  println(SimpleRNG.nonNegativeInt(new SimpleRNG(26051989)))
}

