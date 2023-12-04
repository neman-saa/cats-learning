package com.rockthejvm.effect_concurrency

import cats.effect.{IO, IOApp}

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}
import scala.util.{Failure, Success, Try}

object AsyncIOs extends IOApp.Simple {

  // you can run ios on fibers without to manually manage the fiber lifecycle

  val threadPool: ExecutorService = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(threadPool)
  type CallBack[A] = Either[Throwable, A] => Unit
  def computeMeaningOfLife() = {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computing on some other thread")
    42
  }
  def computeMeaningOfLifeEither(): Either[Throwable, Int] = Try {
    computeMeaningOfLife()
  }.toEither

  def computeMOLonThreadPool: Unit = threadPool.execute(() => computeMeaningOfLife()) // returns unit
  val asyncMolIO: IO[Int] = IO.async_ { cb => //Cats effect thread blocks(semantically) until this callback is invoked by some other thread
    threadPool.execute {() => // computation not managed by CE
      Thread.sleep(1000)
      val result = computeMeaningOfLifeEither()
      cb(result) //CE thread is notified with the result
    }
  }

  val asyncMolIO2: IO[Int] = IO.async_ { cb => //Cats effect thread blocks(semantically) until this callback is invoked by some other thread
    threadPool.execute { () => // computation not managed by CE
      Thread.sleep(1500)
      val result = computeMeaningOfLifeEither()
      cb(result) //CE thread is notified with the result
    }
  }

  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] = {
    IO.async_ {cb =>
      ec.execute { () =>
        val result = Try(computation()).toEither
        cb(result)
      }
    }
  }

  lazy val molFuture: Future[Int] = Future {computeMeaningOfLife()} (ec)

  def convertFutureToIO[A](value: => Future[A]): IO[A] = IO.async_ { cb =>
    value onComplete {
      case Success(value) => cb(Right(value))
      case Failure(exception) => cb(Left(exception))
    }
  }
  override def run: IO[Unit] = ???
}