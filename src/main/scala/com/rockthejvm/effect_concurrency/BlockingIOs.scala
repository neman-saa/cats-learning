package com.rockthejvm.effect_concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.Utils.IOOps

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

object BlockingIOs extends IOApp.Simple {

  val testing = for {
    fib1 <- (IO.sleep(1.second) >> IO("First ended").debug).start
    fib2 <- (IO.sleep(1.second) >> IO("Second ended").debug).start
    _ <- fib1.join
    _ <- fib2.join
  } yield ()

  val sameSleeps = for {
    _ <- IO.sleep(1.second).debug
    _ <- IO.sleep(1.second).debug
  } yield ()

  import cats.syntax.parallel._
  import cats.instances.list._

  def toIO(a: String): IO[Int] = IO{Thread.sleep(1000); a.length}.debug
  val strings: List[String] = List("gplpom", "[oooo", "gl,l")

  val testTraverse: IO[Unit] = strings.parTraverse(toIO).void
  override def run: IO[Unit] = testTraverse
}
