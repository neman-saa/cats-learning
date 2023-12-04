package com.rockthejvm.effect_concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.kernel.{Outcome, Resource}
import cats.effect.{IO, IOApp}
import com.rockthejvm.Utils.IOOps

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.DurationInt

object Resources extends IOApp.Simple {

  //use-case: manage a connection lifecycle
  class Connection(url: String) {
    def open: IO[String] = IO(s"opening connection on $url").debug

    def closingConnection: IO[String] = IO(s"closing connection on $url").debug
  }

  val asyncFetchUrl: IO[Unit] = for {
    fib <- (new Connection("lol@gmail.com").open *> IO.sleep(1000000000.second)).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  val correctSyncFetchUrl: IO[Unit] = for {
    conn <- IO(new Connection("lol@gmail.com"))
    fib <- (conn.open *> IO.sleep(1000000000.second)).onCancel(conn.closingConnection.void).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  val bracketFetchUrl: IO[Unit] = IO(new Connection("lol@gmail.com")).bracket(_.open *> IO.sleep(Int.MaxValue.second))(_.closingConnection.void)

  val bracketProgram: IO[Unit] = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  def readEvery100millis(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) IO(scanner.nextLine()).debug.void >> IO.sleep(100.millis) >> readEvery100millis(scanner)
    else IO.unit

  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def bracketReadFile(path: String): IO[Unit] = {

    val bracket = IO("opening scanner").debug >>
      openFileScanner(path)
        .bracket(readEvery100millis)(scanner => IO("closing scanner").debug >> IO(scanner.close()))
    bracket
  }

  def testing[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join
  } yield result // all of computations are on different threads

  def connFromConfig(path: String): IO[Unit] = openFileScanner(path)
    .bracket(scanner =>
      IO(new Connection(scanner.nextLine())).bracket(_.open.debug >> IO.never)(conn => conn.closingConnection.void))(scanner =>
      IO("closing file").debug >> IO(scanner.close()))

  val connectionResource: Resource[IO, Connection] = Resource.make(IO(new Connection("lol.com")))(conn => conn.closingConnection.void)
  val resourceFetchUrl: IO[Unit] = for {
    fib <- connectionResource.use(conn => conn.open >> IO.never).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  val simpleResource: IO[String] = IO("some resource")
  val usingResource: String => IO[String] = (string: String) => IO(s"using $string").debug
  val releaseResource: String => IO[Unit] = (string: String) => IO(s"finalizing the string $string").debug.void

  val usingResourceWithBracket: IO[String] = simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource: IO[String] = Resource.make(simpleResource)(releaseResource).use(usingResource)

  def scannerResource(path: String): IO[Scanner] = IO(new Scanner(new FileReader(new File(path))))

  val scannerReleaseResource: Scanner => IO[Unit] = (scanner: Scanner) => IO("closing scanner").debug >> IO(scanner.close())
  val scannerUsageResource: Scanner => IO[Unit] = (scanner: Scanner) => readEvery100millis(scanner)

  def readFileWithResources(path: String): IO[Unit] = Resource.make(scannerResource(path))(scannerReleaseResource).use(scannerUsageResource)

  def connFromConfResource(path: String): Resource[IO, Connection] =
    Resource.make(IO("opening file").debug >> openFileScanner(path))(scanner => IO("closing file").debug >> IO(scanner.close()))
      .flatMap(scanner => Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.closingConnection.void))

  def connFromConfResourceClean(path: String): Resource[IO, Connection] = for {
    scanner <- Resource.make(IO("opening file").debug >> openFileScanner(path))(scanner => IO("closing file").debug >> IO(scanner.close()))
    conn <- Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.closingConnection.void)
  } yield conn

  val openConnection: IO[String] = connFromConfResourceClean("src/main/resources/connection.txt").use(conn => conn.open) // connection and file will close automatically

  val finalizerOnDefaultIO: IO[String] = IO("String").debug.guarantee(IO("freeing resource").debug.void)
  val final2: IO[String] = IO("String").debug.guaranteeCase {
    case Succeeded(fa) => fa.debug.void
    case Canceled() => IO("StringFinal").debug.void
    case Errored(e) => IO.raiseError(e).debug.void
  }

  override def run: IO[Unit] = openConnection.void
}
