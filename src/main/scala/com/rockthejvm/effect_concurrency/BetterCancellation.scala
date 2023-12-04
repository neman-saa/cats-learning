package com.rockthejvm.effect_concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.Utils.IOOps

import scala.concurrent.duration.DurationInt
import scala.io.StdIn

object BetterCancellation extends IOApp.Simple {

  /*
  Cancellation ios:
  - fiber cancellation
  - IO.race and others
  - manual cancellation
   */

  val cancelIO = IO("glpmhpkm").debug >> IO.canceled >> IO.unit // all things after IO.canceled will not be evaluated

  // uncancelable
  // example: online store, payment processor
  // payment must NOT be canceled

  val specialPaymentSystem =
    (IO("Payment running, do not cancel me...").debug >> IO.sleep(1.second) >> IO("Payment completed").debug).onCancel(IO("MEGA CANCEL OF DOOM!").debug.void)

  val atomicPayment = IO.uncancelable(_ => specialPaymentSystem) // "masking"
  val atomicV2 = specialPaymentSystem.uncancelable

  val noCancellationOfDoom = for {
    fib <- atomicV2.start
    _ <- IO.sleep(500.millis) >> IO("attempting cancellation...").debug >> fib.cancel
    _ <- fib.join
  } yield ()

  val inputPassword = IO("Input password").debug >> IO(StdIn.readLine())
  val verifyPassword = (pw: String) => IO("Verifying").debug >> IO.sleep(2.second) >> IO(pw == "Million100000")
  val authFlow = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication failed, tray again").debug.void) // unmasking inputPassword -> cancelable
      verified <- verifyPassword(pw) // this is not cancelable
      _ <- if (verified) IO("Authentication successful").debug // not cancellable
      else IO("Authentication failed").debug //musking only ignore cancel signal, but if after exists unmasked part then cancellation will happen on this part, it means that there are no sense to make unmusked value after musked values
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.start
    _ <- IO.sleep(3.second) >> IO("Authentication timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()


  override def run: IO[Unit] = authProgram
}
