package com.rockthejvm.effects_and_io

import cats.effect.{IO, IOApp}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object IO_traversal extends IOApp.Simple {

  def heavyComputation(string: String): Future[Int] = Future({
    Thread.sleep(1000); string.split(" ").length
  })

  val workLoad: List[String] = List("I am learning cats effect", "Scala is awesome", "Looking forward to some awesome staff")
  val futures: List[Future[Int]] = workLoad.map(heavyComputation)

  import cats.instances.list._
  import cats.Traverse

  val listTraverse: Traverse[List] = Traverse[List]
  val traversedList: Future[List[Int]] = listTraverse.sequence(futures)

  def computeAsIO(string: String): IO[Int] = IO({
    Thread.sleep(1000)
    string.split(" ").length
  }).map(x => {println(x, Thread.currentThread().getName); x})

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.sequence(ios)

  import cats.syntax.parallel._

  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO) // all the computations of list elements occur on different threads
  override def run: IO[Unit] = parallelSingleIO.void

  
}
