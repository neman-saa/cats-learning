package com.rockthejvm.effects_and_io

import cats.effect.IO
import cats.effect.unsafe.implicits.global

object IO_errorHandling extends App {
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("Proper failure")) // throws error if run
  val dealWith = aFailure.handleErrorWith({
    case _: RuntimeException => IO(42)
  })

  //turn into an either

  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt
  val mapped = effectAsEither.map({
    case Left(value) => 42
    case Right(number) => number * 2
  })

  val redeem = aFailure.redeem(er => 42, value => value)
  println(mapped.unsafeRunSync())
  println(redeem.unsafeRunSync())
  println(5)
}
