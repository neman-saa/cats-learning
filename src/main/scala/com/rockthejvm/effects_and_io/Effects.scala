package com.rockthejvm.effects_and_io

object Effects extends App {

  //pure functional programming
  //substitution
  //referential transparency

  def combine(a: Int, b: Int): Int = a + b
  val five1 = combine(2, 3)
  val five2 = 2 + 3
  val five3 = 5

  val print: Unit = println("Cats Effect")
  val print2: Unit = () //not the same

  var int = 5
  val changing: Unit = int += 1

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(unsafeRun()).unsafeRun())
  }
  /*
  example MyIO data type - it IS  an effect type
  - describes any computation that might produce side effects
  - calculate a value of type A if its successful
  - side effects are required for evaluation of this type
  - creation of MyIO does NOT produce the side effect on construction
  */

  val io: MyIO[Int] = MyIO(() => {
    println("Something")
    42
  })

  val clock: MyIO[Long] = MyIO(() => System.currentTimeMillis())
  def measureIO[A](computation: MyIO[A]): MyIO[Long] = for {
    startTime <- clock
    _ <- computation
    finishTime <- clock
  } yield finishTime - startTime
}
