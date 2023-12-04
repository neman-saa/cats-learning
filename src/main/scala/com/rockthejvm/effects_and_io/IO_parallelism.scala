package com.rockthejvm.effects_and_io

import cats.Parallel
import cats.effect.{ExitCode, IO, IOApp}

object IO_parallelism extends IOApp {
  //IOs are usually sequential

  val io1: IO[String] = IO({
    println(s"[${Thread.currentThread().getName}] 1");
    "1"
  })
  val io2: IO[String] = IO({
    println(s"[${Thread.currentThread().getName}] 2");
    "2"
  })

  val composed: IO[String] = for {
    a <- io1
    b <- io2
  } yield {
    println(s"[${Thread.currentThread().getName}] ($a, $b, 3)"); s"$a , $b,  3"
  }

  //converts sequential io to parallel io

  import cats.effect.implicits._
  import cats.syntax.apply._


  val parIO1: IO.Par[String] = Parallel[IO].parallel(io1)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(io2)

  val parallelComposed: IO.Par[String] =
    (parIO1, parIO2)
      .mapN((x, y) => {
        println(s"[${Thread.currentThread().getName}] $x, $y 3"); s"$x $y 3"
      })// y evaluates before x, idk why
  val backToSequential: IO[String] = Parallel[IO].sequential(parallelComposed) // but continue evaluate on different threads

  import cats.syntax.parallel._

  val transformToParallelAndBack: IO[String] = (io1, io2).parMapN((x, y) => {
    println(s"[${Thread.currentThread().getName}], $x $y 3"); s"$x $y, 3"
  }) // still works on different threads

  // regarding failure
  val aFailure: IO[String] = IO.raiseError(new RuntimeException("Failure!"))
  val parallelWithFailure: IO[String] = (io1, aFailure).parMapN(_ + _)
  //The first result that fails gives the failure result
  // Can handle failures only before parMapN method
  override def run(args: List[String]): IO[ExitCode] = transformToParallelAndBack.as(ExitCode.Success) // back to sequential, but continue evaluate on different threads

}
