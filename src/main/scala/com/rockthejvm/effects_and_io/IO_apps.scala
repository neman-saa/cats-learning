package com.rockthejvm.effects_and_io

import cats.effect.{ExitCode, IO, IOApp}
import IO_apps.program

import scala.io.StdIn

object IO_apps extends IOApp {
  def program = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
  } yield println(line1 + line2)
  override def run(args: List[String]): IO[ExitCode] = program.as(ExitCode.Success)
}

object SimpleApp extends IOApp.Simple {
  override def run: IO[Unit] = program

}

