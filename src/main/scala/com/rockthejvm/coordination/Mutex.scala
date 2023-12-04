package com.rockthejvm.coordination

import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Ref
import cats.effect.{Deferred, IO, IOApp}

import scala.concurrent.duration.DurationInt
import scala.util.Random
import com.rockthejvm.Utils._

import scala.collection.immutable.Queue
import scala.collection.{immutable, mutable}

abstract class Mutex {
  def acquire: IO[Unit]

  def release: IO[Unit]
}

object Mutex {
  case class State(isLocked: Boolean, queue: immutable.Queue[Deferred[IO, Unit]])
  def create: IO[Mutex] = Ref[IO].of(State(isLocked = false, Queue())).map (ref => new Mutex {
    override def acquire: IO[Unit] = for {
      state <- ref.get
      mbWait <- Deferred[IO, Unit]
      _ <- if (state.isLocked) ref.update(state => state.copy(queue = state.queue.enqueue(mbWait))) >> mbWait.get
      else ref.update(state => state.copy(isLocked = true))
    } yield ()

    override def release: IO[Unit] = for {
      state <- ref.get
      _ <-
        if (!state.isLocked) IO.unit
        else {
          if (state.queue.isEmpty) ref.set(State(isLocked = false, Queue()))
          else state.queue.head.complete() >> ref.update(state => state.copy(queue = state.queue.tail))
        }
    } yield ()
  })
}

object MutexPlayground extends IOApp.Simple {
  def criticalTask: IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))
  def createNonLockingTask(id: Int): IO[Int] = for {
    _ <- IO(s"[task$id] working...").debug
    res <- criticalTask
    _ <- IO(s"[task$id] completed: $res").debug
  } yield res

  import cats.syntax.parallel._
  def demoNonLocking: IO[List[Int]] = (1 to 10).toList.parTraverse(createNonLockingTask)
  def createLockingTask(id: Int, mutex: Mutex): IO[Int] = for {
    _ <- IO(s"[task$id] working...").debug
    res <- criticalTask
    _ <- IO(s"[task$id] waiting for permission...").debug
    _ <- mutex.acquire //blocks if the mutex has been acquired by some other fiber
    _ <- IO(s"[task$id] completed: $res").debug
    _ <- mutex.release
    _ <- IO(s"[task$id] lock removed")
  } yield res

  def demoLockingTasks = for {
    mutex <- Mutex.create
    results <- (1 to 11).toList.parTraverse(createLockingTask(_, mutex)).guaranteeCase{
      case Succeeded(_) => IO.unit
      case _ => mutex.release >> IO(-1)
    }
  } yield results
  import cats.syntax.apply._
  def testing = {for {

    deferred <- Deferred[IO, Int]
    _ <- IO.sleep(1.second) >> deferred.complete(2)
    fib1 <- deferred.get.start
    res1 <- fib1.join
    fib2 <- deferred.get.start
    res2 <- fib2.join
  } yield (res2, res1).mapN(_ + _)} flatMap {
    case (Succeeded(fa)) => fa
    case _ => IO(5)
  }

  override def run: IO[Unit] = demoLockingTasks .debug.void
}