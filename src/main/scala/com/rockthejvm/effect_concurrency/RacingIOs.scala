package com.rockthejvm.effect_concurrency

import cats.effect.kernel.Outcome
import cats.effect.{FiberIO, IO, IOApp}
import com.rockthejvm.Utils.IOOps

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object RacingIOs extends IOApp.Simple {
  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] = {
    (IO("Starting computation").debug >> IO.sleep(duration) >> IO("Computation done").debug >> IO(value))
      .onCancel(IO("Computation canceled").debug.void)
  }

  def testRace: IO[String] = {
    val meaningOLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.second)
    val first: IO[Either[Int, String]] = IO.race(meaningOLife, favLang) // first one will be a result, second will be canceled

    first.flatMap({
      case Left(value) => IO(s"Meaning of life: $value won")
      case Right(value) => IO(s"Favorite lang won: $value")
    })
  }

  def testRacePair: IO[Outcome[IO, Throwable, _ >: String with Int]] = {
    val meaningOLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.second)
    val first = IO.racePair(meaningOLife, favLang) // first is in outcome second is in a fiber

    first.flatMap({
      case Left((outMoL, fibLang)) => IO(s"Meaning of life: $outMoL won").debug >> fibLang.join
      case Right((fibMol, outLang)) => IO(s"Favorite lang won: $outLang").debug >> fibMol.join.debug
    })
  }

  val ioTimeout  : IO[String] = IO("").timeout(1.second)
  override def run: IO[Unit] = testRacePair.debug.void
}
