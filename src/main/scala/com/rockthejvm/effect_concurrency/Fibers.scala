package com.rockthejvm.effect_concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp}

object Fibers extends IOApp.Simple {

  val number: IO[Int] = IO.pure(42)
  val string: IO[String] = IO.pure("String")

  import com.rockthejvm.Utils._

  def simpleComposition: IO[(Int, String)] = for {
    a <- number.debug
    b <- string.debug
  } yield (a, b)

  //introduce the Fiber

  def createFiber: Fiber[IO, Throwable, String] = ??? // almost impossible to create fibers manually

  //the fiber is not actually started , but the fiber allocation is wrapped in another effect
  val aFiber: IO[Fiber[IO, Throwable, Int]] = number.debug.start // can produce side effect, due to this second IO wrapper exists

  def differentThreadIOs: IO[Unit] = for {
    _ <- aFiber
    _ <- string.debug
  } yield ()

  //joining a fiber
  def  runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join // an effect which waits for fiber terminates
  } yield result

  def someResultFromAnotherThread: IO[Int] = runOnSomeOtherThread(number).flatMap({
    case Succeeded(fa) => fa
    case Errored(_) => IO(3)
    case Canceled() => IO(3)
  })

  def testCancel: IO[Outcome[IO, Throwable, Unit]] = {
    val task = IO(println("Starting")) >> IO(Thread.sleep(1000)) >> IO(println("done"))
    val taskWithCancellationHandler = task.onCancel(IO("task cancelled").debug.void)

    for {
      fib <- task.start
      _ <- IO(Thread.sleep(500)) >> IO(println("cancelling")).debug
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }
  //possible outcomes succeeded,  failure, cancelled
  override def run: IO[Unit] = runOnSomeOtherThread(number) // IO(Succeeded(IO(42)))
    .debug.void

  def runOnAnotherThread[A](io: IO[A]): IO[A] = {
    val result = for {
      fib <- io.start
      result <- fib.join
    } yield result

    result.flatMap{
      case Succeeded(value) => value
      case _ => IO.raiseError(new Exception("Cancelled or Failed"))
    }
  }

  def twoIOsOnAnotherThreads[A, B](io1: IO[A], io2: IO[B]): IO[(A, B)] = {
    val results = for {
      fib1 <- io1.start
      fib2 <- io2.start
      result1 <- fib1.join
      result2 <- fib2.join
    } yield (result1, result2)

    results.flatMap{
      case (Succeeded(value), Succeeded(fa)) => value.flatMap(x => fa.map(y => (x, y)))
      case (Errored(er), _) => IO.raiseError(er)
      case (_, Errored(er)) => IO.raiseError(er)
      case (Canceled(), Canceled()) => IO.raiseError(new RuntimeException("Both cancelled"))
    }
  }

  def withTimeout[A](io: IO[A]): IO[A] = {
    val result = for {
      fib <- io.start
      _ <- IO(Thread.sleep(1000)) >> fib.cancel
      result <- fib.join
    } yield result

    result.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Cancelled"))
    }
  }
}
