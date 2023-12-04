package com.rockthejvm.coordination

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp, Ref}
import com.rockthejvm.Utils.IOOps

object Refs extends IOApp.Simple {

  val atomicMol: IO[Ref[IO, Int]]  = Ref[IO].of(42)
  val atomicMol_v2: IO[Ref[IO, Int]] = IO.ref(42)

  //modifying is an effect
  val increased: IO[Unit] = atomicMol.flatMap(ref => ref.set(43)) //thread safe, increase value in atomicMol
  val fMol: IO[Unit] = atomicMol.flatMap(_.update(_ * 10))
  val mol: IO[Int] = atomicMol.flatMap(_.get)
  val updateAndGet: IO[Int] = atomicMol.flatMap(_.updateAndGet(_ * 10)) //get old set new
  val setAndGet: IO[Int] = atomicMol.flatMap(_.getAndSet(43 ))

  def demoConcurrentWork(): IO[Unit] = {
    import cats.syntax.parallel._
    def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {
      for {
        _ <- IO(s"Counting words for $workload: ${workload.split(" ").length}").debug
        newCount <- total.updateAndGet(_ + workload.split(" ").length)
        _ <- IO(s"New count: $newCount").debug
      } yield()
    }

    for {
      initial <- Ref[IO].of(0)
      _ <- List("g g g ", "g g g g", "g, g, g, g, g").map(task(_, initial)).parSequence
    } yield()
  }
  override def run: IO[Unit] = ???

  //ref should be объявлена внутри for comp : for {ref <- Ref[IO].of(5)}, but not like: val ref = Ref[IO].of(4)

  val testing = ???
  
}
