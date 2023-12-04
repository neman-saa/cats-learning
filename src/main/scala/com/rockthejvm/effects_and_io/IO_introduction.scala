package com.rockthejvm.effects_and_io

import cats.effect.IO

object IO_introduction extends App {
  val firstIO: IO[Int] = IO.pure(42) // arg that should not have side effect
  val delayedIO: IO[Int] = IO.delay({
    println(42);
    42
  }) // can produce side effect and gives integer
  val delayedApply: IO[Int] = IO({
    println(42);
    42
  })

  import cats.effect.unsafe.implicits.global

  delayedApply.unsafeRunSync()

  import cats.syntax.apply._

  val mappedN = (IO(42), IO(42).map(_ * 2)).mapN(_ + _)
  mappedN.unsafeRunSync()

  def productR[A, B](a: IO[A], b: IO[B]): IO[B] = for {
    _ <- a
    b <- b
  } yield b

  def productL[A, B](a: IO[A], b: IO[B]): IO[A] = for {
    a <- a
    _ <- b
  } yield a

  def forever[A](a: IO[A]): IO[A] = {
    a.flatMap(_ => forever(a))
  }

  def convert[A, B](a: IO[A], value: B): IO[B] = a.map(_ => value)

  def discard[A](a: IO[A]) = a.map(_ => ())

  def sum(n: Int): IO[Int] =
    if (n == 0) IO(0)
    else (IO(n), sum(n - 1)).mapN(_ + _)
  def fibonacci(n: Int, prev1: BigInt = 0, prev2: BigInt = 1, counter: Int = 0): BigInt = {
    if (counter == n) prev1
    else fibonacci(n, prev2, prev1 + prev2, counter + 1)
  }

  def fib(n: Int, a: Long, b: Long): IO[Long] =
      if (n > 0)
        IO.defer(fib(n - 1, b, a + b))
      else
        IO.pure(a)
  (1 to 100).foreach(i => println(i, fib(i, 1, 1).unsafeRunSync()))
}
