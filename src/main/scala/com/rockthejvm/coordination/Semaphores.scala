package com.rockthejvm.coordination

import cats.effect.std.Semaphore
import cats.effect.{IO, IOApp}
import com.rockthejvm.Utils.IOOps

import scala.concurrent.duration.DurationInt
import scala.util.Random
object Semaphores extends IOApp.Simple {
  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2)

  def doWOrkAfterLoginIn: IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))
  def login(id: Int, sem: Semaphore[IO]) = for {
    _ <- IO(s"[session $id] waiting to login...").debug
    _ <- sem.acquire
    _ <- IO(s"[session $id] logged in, working...").debug
    res <- doWOrkAfterLoginIn
    _ <- IO(s"Done: $res, logging out").debug
    _ <- sem.release
  } yield ()

  def demoSemaphore = for {
    sem <- Semaphore[IO](2)
    user1 <- login(1, sem).start
    user2 <- login(2, sem).start
    user3 <- login(3, sem).start
    _ <- user1.join
    _ <- user2.join
    _ <- user3.join
  } yield ()
  override def run: IO[Unit] = demoSemaphore
}
