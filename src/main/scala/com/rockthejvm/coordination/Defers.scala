package com.rockthejvm.coordination

import cats.effect.kernel.Outcome
import cats.effect.{Deferred, FiberIO, IO, IOApp, OutcomeIO, Ref}
import com.rockthejvm.Utils.IOOps

import scala.concurrent.duration.DurationInt

object Defers extends IOApp.Simple {

  val aDeferred: IO[Deferred[IO, Int]] = Deferred[IO, Int]
  val reader: IO[Int] = aDeferred.flatMap(deferred => deferred.get)
  val aWriter: IO[Int] = aDeferred.flatMap(_.get)

  def demoDeferred: IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[consumer] waiting for result").debug
      meaningOfLife <- signal.get // block thread until get a value
      _ <- IO(s"[consumer] got a result: $meaningOfLife").debug
    } yield()

    def producer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[producer] crunching numbers").debug
      _ <- IO.sleep(1.second)
      _ <- IO("[producer] complete: 42").debug
      meaningOfLife <- IO(42)
      _ <- signal.complete(meaningOfLife)
    } yield ()

    for {
      signal <- Deferred[IO, Int]
      fibConsumer <- consumer(signal).start
      fibProducer <- producer(signal).start
      _ <- fibConsumer.join
      _ <- fibProducer.join
    } yield()
  }

  val fileParts: List[String] = List("I lov", "e Scal", "a with", "Cats eff", "ect!<EOF>")
  def fileNotifierWithRef: IO[Unit] = {
    import cats.syntax.traverse._
    def downloadFile(contentRef: Ref[IO, String]) = {
      fileParts.map {part =>
        IO(s"got $part").debug >> IO.sleep(1.second) >> contentRef.update(_ + part)
      }.sequence.void
    }

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      file <- contentRef.get
      _ <-
        if (file.endsWith("<EOF>")) IO("Downloading complete").debug
        else IO("Downloading").debug >> IO.sleep(500.millis) >> notifyFileComplete(contentRef)
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      fibDownloader <- downloadFile(contentRef).start
      fibNotifier <- notifyFileComplete(contentRef).start
      _ <- fibDownloader.join
      _ <- fibNotifier.join
    } yield ()
  }

  def fileNotifierWithDefer: IO[Unit] = {
    import cats.syntax.traverse._
     def notifyFileComplete(signal: Deferred[IO, String]) = for {
       _ <- IO(s"[notifier] downloading...").debug
       _ <- signal.get
       _ <- IO(s"[notifier] downloading completed").debug
     } yield ()

    def fileDownloader(part: String, contentRef: Ref[IO, String], signal: Deferred[IO, String]) = for {
      _ <- IO(s"got $part").debug
      _ <- IO.sleep(1.second)
      lastPart <- contentRef.updateAndGet(_ + part)
      _ <- if (lastPart.contains("<EOF>")) signal.complete(lastPart)
      else IO.unit
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      signal <- Deferred[IO, String]
      notifierFib <- notifyFileComplete(signal).start
      fileDownloaderFib <- fileParts.map(fileDownloader(_, contentRef, signal)).sequence.start
      _ <- notifierFib.join
      _ <- fileDownloaderFib.join
    } yield ()
  }

  def clockCounter: IO[Unit] = {
    def counter(ref: Ref[IO, Int], signal: Deferred[IO, Int]): IO[Unit] = for {
      last <- ref.get
      _ <- if (last == 11) signal.complete(last)
      else IO.sleep(1.second) >> IO(s"Now is: $last").debug >> ref.set(last + 1) >> counter(ref, signal)
    } yield ()

    def completer(signal: Deferred[IO, Int]) = for {
      _ <- IO("Waiting...").debug
      _ <- signal.get
      _ <- IO("Time`s up").debug
    } yield ()

    for {
      ref <- Ref[IO].of(0)
      signal <- Deferred[IO, Int]
      counter <- counter(ref, signal).start
      completer <- completer(signal).start
      _ <- counter.join
      _ <- completer.join
    } yield ()
  }

  def testing(): IO[OutcomeIO[Int]] = for {
    deferred <- Deferred[IO, Int]
    getter <- deferred.get.start
    first <- (IO.sleep(1.second) >> deferred.complete(1) >> IO("first").debug).start
    second <- (IO.sleep(1.second) >> deferred.complete(2) >> IO("second").debug).start
    _ <- first.join
    _ <- second.join
    result <- getter.join
  } yield result

  def racePairViaDeferred[A, B](firstComputation: IO[A], secondComputation: IO[B]): IO[Either[(OutcomeIO[A], FiberIO[B]), (FiberIO[A], OutcomeIO[B])]] = IO.uncancelable { poll =>
    for {
      signal <- Deferred[IO, Either[OutcomeIO[A], OutcomeIO[B]]]
      fib1 <- firstComputation.guaranteeCase(s => signal.complete(Left(s)).void).start
      fib2 <- secondComputation.guaranteeCase(s => signal.complete(Right(s)).void).start
      result <- poll(signal.get).onCancel {
        for {
          _ <- fib1.cancel
          _ <- fib2.cancel
        } yield ()
      }
    } yield result match {
      case Left(value) => Left(value, fib2)
      case Right(value) => Right(fib1, value)
    }
  }
  lazy val first = IO.sleep(1.second) >> IO(42)
  lazy val second = IO.sleep(500.millis) >> IO(42)

  def testing2(): IO[Outcome[IO, Throwable, Int]] = {
    import cats.syntax.apply._
    for {
      fib1 <- (IO.sleep(2.second) >> IO(42).debug).start
      fib2 <- (IO.sleep(2.second) >> IO(43).debug).start
      res1 <- fib1.join
      res2 <- fib2.join
    } yield (res1, res2).mapN(_ + _)
  }


  override def run: IO[Unit] = testing2().debug.void
}
