package zioborrar.repository

import scalaz.zio._
import scalaz.zio.clock.Clock
import zioborrar.MyApp.{AuthId, AuthSecret, CredentialId}

import scala.concurrent.Future
import scala.language.postfixOps

//import scala.concurrent.duration._

import scalaz.zio.duration._

//import scala.io.StdIn
//import scala.{Console => SConsole}

trait Repository {
  val repository: Repository.Service[Any]
}

object Repository {

  trait Service[R] {

    def findName(authId: AuthId): TaskR[Repository, String]

    def findNameWithDelay(authId: AuthId): ZIO[Repository with Clock, Throwable, String]

    def findSecret(authId: AuthId): TaskR[Repository, AuthSecret]

    def findCredentialId(authId: AuthId): TaskR[Repository, CredentialId]

  }

  trait Live extends Repository {

    private val AUTH_ID_TO_NAME: Map[AuthId, String] = Map(
      "420606" -> "Munch"
    )

    private val AUTH_ID_TO_SECRET_ID = Map[AuthId, AuthSecret](
      "420606" -> "skrik"
    )

    private val AUTH_ID_TO_CREDENTIAL_ID = Map[AuthId, CredentialId](
      "420606" -> 7
    )

    def selectTask[K, V](table: Map[K, V])(k: K): Task[V] = table.get(k) match {
      case Some(value) => Task.succeed(value)
      case None => Task.fail(new Exception(s"could not find value for key $k")).orDie
    }

    def selectFuture[K, V](table: Map[K, V])(k: K): Future[V] = table.get(k) match {
      case Some(value) => Future.successful(value)
      case None => Future.failed(new Exception(s"could not find value for key $k"))
    }

    def selectFromFuture[K, V](table: Map[K, V])(k: K) = ZIO.fromFuture(_ => selectFuture(table)(k))

    def selectWithDelay[K, V](table: Map[K, V])(k: K): ZIO[Any with Clock, Throwable, V] = ZIO.fromFuture(_ => selectFuture(table)(k)).delay(5 seconds)

    val repository: Service[Any] = new Service[Any] {

      final def findName(authId: AuthId): Task[String] = selectFromFuture(AUTH_ID_TO_NAME)(authId)

      final def findNameWithDelay(authId: AuthId): ZIO[Any with Clock, Throwable, String] = selectWithDelay(AUTH_ID_TO_NAME)(authId)

      override def findSecret(authId: AuthId): Task[AuthSecret] = selectTask(AUTH_ID_TO_SECRET_ID)(authId)

      override def findCredentialId(authId: AuthId): Task[CredentialId] = selectTask(AUTH_ID_TO_CREDENTIAL_ID)(authId)


//      /**
//        * Prints text to the repository.
//        */
//      final def putStr(line: String): UIO[Unit] =
//        putStr(SConsole.out)(line)
//
//      final def putStr(stream: PrintStream)(line: String): UIO[Unit] =
//        IO.effectTotal(SConsole.withOut(stream) {
//          SConsole.print(line)
//        })
//
//      /**
//        * Prints a line of text to the repository, including a newline character.
//        */
//      final def putStrLn(line: String): UIO[Unit] =
//        putStrLn(SConsole.out)(line)
//
//      final def putStrLn(stream: PrintStream)(line: String): UIO[Unit] =
//        IO.effectTotal(SConsole.withOut(stream) {
//          SConsole.println(line)
//        })
//
//      /**
//        * Retrieves a line of input from the repository.
//        */
//      final val getStrLn: ZIO[Any, IOException, String] =
//        getStrLn(SConsole.in)
//
//      /**
//        * Retrieves a line of input from the repository.
//        * Fails with an [[java.io.EOFException]] when the underlying [[java.io.Reader]]
//        * returns null.
//        */
//      final def getStrLn(reader: Reader): IO[IOException, String] =
//        IO.effect(SConsole.withIn(reader) {
//          val line = StdIn.readLine()
//          if (line == null) {
//            throw new EOFException("There is no more input left to read")
//          } else line
//        })
//          .refineOrDie {
//            case e: IOException => e
//          }
    }
  }

  object Live extends Live

}
