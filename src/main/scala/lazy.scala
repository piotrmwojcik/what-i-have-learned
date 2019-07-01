import scala.annotation.tailrec

sealed trait Stream[+A] {
  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this.foldRight((z, Stream(z)))((a, p0) => {
    lazy val p1 = p0
    val b = f(a, p1._1)
    (b, Stream.cons(b, p1._2))
  })._2

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) ||b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  //def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def map[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  /*def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }*/

  def take(n: Int): Stream[A] = Stream.unfold((this, n)) {
    case (Cons(h, _), 1) => Some((h(), (Stream.empty, 0)))
    case (Cons(h, t), k) if k > 1 => Some((h(), (t(), k - 1)))
    case _ => None
  }

  /*def takeWhile(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)*/

  def takeWhile(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = Stream.unfold((this, s2)) {
    case (Empty, Empty) => None
    case (Empty, Cons(h2, t2)) => Some(f(None, Some(h2())) -> (Stream.empty, t2()))
    case (Cons(h1, t1), Empty) => Some(f(Some(h1()), None) -> (t1(), Stream.empty))
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = zipWithAll(s2)(_ -> _)

  def filter[B](p: A => Boolean): Stream[A] = foldRight(Stream.empty  [A])((a, b) => if (p(a)) Stream.cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => f(a) append b)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /*val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, Stream.from(n + 1))

  def fibs: Stream[Int] = {
    def go(p: Int, n: Int): Stream[Int] = Stream.cons(p, go(n, p + n))
    go(0, 1)
  }*/

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Stream.empty[A]
    case Some((v, zz)) => Stream.cons(v, unfold (zz)(f))
  }

  val ones: Stream[Int] = unfold(1)((s: Int) => Some((1, s)))

  def constant[A](a: A): Stream[A] = unfold(a)((s: A) => Some((a, s)))

  def from(n: Int): Stream[Int] = unfold(n)((s: Int) => Some((s + 1, s)))

  def fibs: Stream[Int] = unfold((0, 1)){case (s1, s2) => Some((s1, (s2, s1 + s2)))}
}

object test extends App {
  println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)
  println(Stream(1, 2, 3, 4, 5, 6, 7).zipAll(Stream(1, 2, 3)).toList)
  println(Stream(1, 2, 3, 4, 5, 6).toList)
  println(Stream(1, 2, 3, 4, 5, 6).take(3).toList)
  println(Stream(1, 2, 3, 4, 5, 6).take(0).toList)
  println(Stream(1, 2, 3, 4, 5, 6).take(1).toList)
  println(Stream(1, 2, 3, 4, 5, 6).append(Stream(10)).toList)
  println(Stream(1, 2, 3, 4, 5, 6).map(_ + 10).toList)
  println(Stream(1, 2, 3, 4, 5, 6).flatMap(Stream(1, _)).toList)
  println(Stream(1, 2, 3, 4, 5, 6).filter(_ % 3 == 0).toList)
  println(Stream(1, 1, 2, 3, 4, 5, 6, 1).takeWhile(_ < 4).toList)
  println(Stream.ones.take(10).toList)
  println(Stream.constant(5).take(10).toList)
  println(Stream.from(1).take(10).toList)
  println(Stream.fibs.take(10).toList)

  def unfoldf(s: Int): Option[(Int, Int)] = if (s > 10) None else Some((s, s + 1))

  println(Stream.unfold(1)(unfoldf).take(10).toList)

  //println(traverse(s1)(foo))
  //println(traverse(s2)(foo))
}
