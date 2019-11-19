import java.time.Instant
import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import scala.concurrent.duration.TimeUnit


object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, unit: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    new Future[C] {
      def isDone = isDone

      def get: C = get()

      def get(timeout: Long, unit: TimeUnit) = {
        val ts = Instant.now.toEpochMilli
        val af = a(es).get(timeout, unit)
        val duration = unit.convert(Instant.now.toEpochMilli - ts, unit)
        val bf = b(es).get(timeout - duration, unit)
        f(af, bf)
      }

      def isCancelled: Boolean = isCancelled

      def cancel(evenIfRunning: Boolean): Boolean = cancel(evenIfRunning)
    }
  }

  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => es.submit(new Callable[A] {
    def call: A = a(es).get
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))
}

object testApp1 extends App {
  val executorService = java.util.concurrent.Executors.newFixedThreadPool(5)
  val a = Par.lazyUnit({println("starting 1st"); Thread.sleep(4600); println("A"); "a"})
  val b = Par.lazyUnit({println("starting 2nd"); Thread.sleep(4600); println("B"); "b"})
  println(Par.map2(a, b)(Seq(_, _).mkString)(executorService).get(9400, TimeUnit.MILLISECONDS))
}
