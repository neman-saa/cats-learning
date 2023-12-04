package com.rockthejvm.coordination

import cats.effect.std.CountDownLatch
import cats.effect.{Deferred, IO, IOApp, Ref}
import com.rockthejvm.Utils.IOOps
import cats.syntax.parallel._
import cats.syntax.traverse._

import scala.concurrent.duration.DurationInt

object CountDownLatches extends IOApp.Simple {

  def testing = for {
    fib1 <- (IO.sleep(2.second) >> IO("first")).start
    fib2 <- (IO.sleep(1.second) >> IO("second")).start
    res1 <- fib1.join.debug
    res2 <- fib2.join.debug
  } yield ()

  def testing2 = List(1, 2, 3).parTraverse(IO.sleep(1.second) >> IO(_).debug)

  def testingCountdown = for {
    countLatch <- CountDownLatch[IO](5)
    _ <- (1 to 5).toList.traverse(x => IO.sleep(1.second) >> IO(x + "...").debug >> countLatch.release)
    _ <- countLatch.await // blocking
    res <- IO("RESULT")
  } yield res
  override def run: IO[Unit] = testingCountdown.debug.void

  abstract class CountDownLatchc {
    def await: IO[Unit]
    def release: IO[Unit]
  }
  object CountDownLatchc {
    def create(count: Int): IO[CountDownLatchc] = for {
      ref <- Ref[IO].of(count)
      signal <- Deferred[IO, Unit]
    } yield new CountDownLatchc {
      override def await: IO[Unit] = signal.get

      override def release: IO[Unit] = for {
        res <- ref.get
        _ <- if (res == 0) signal.complete() else ref.update(_ - 1)
      } yield ()
    }
  }

}
