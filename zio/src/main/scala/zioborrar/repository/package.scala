package zioborrar

import scalaz.zio.clock.Clock
import scalaz.zio.{TaskR, ZIO}
import zioborrar.MyApp.{AuthId, AuthSecret, CredentialId}
//import zioborrar.repository.Repository
//package scalaz.zio

//import java.io.IOException

//import scalaz.zio.ZIO.{accessM, access}

package object repository extends Repository.Service[Repository with Clock] {

  final val repositoryService: ZIO[Repository with Clock, Exception, Repository.Service[Any]] = ZIO.access(_.repository)

  final def findName(authId: AuthId): ZIO[Repository, Throwable, String] = ZIO.accessM(_.repository.findName(authId))

  final def findNameWithDelay(authId: AuthId): ZIO[Repository with Clock, Throwable, String] = ZIO.accessM(_.repository.findNameWithDelay(authId))

  override def findSecret(authId: AuthId) = ZIO.accessM(_.repository.findSecret(authId))

  override def findCredentialId(authId: AuthId) = ZIO.accessM(_.repository.findCredentialId(authId))

//  final val consoleService: ZIO[Repository, Nothing, Repository.Service[Any]] =
//    ZIO.access(_.repository)
//
//  /**
//    * Prints text to the repository.
//    */
//  final def putStr(line: String): ZIO[Repository, Nothing, Unit] =
//    ZIO.accessM(_.repository putStr line)
//
//  /**
//    * Prints a line of text to the repository, including a newline character.
//    */
//  final def putStrLn(line: String): ZIO[Repository, Nothing, Unit] =
//    ZIO.accessM(_.repository putStrLn line)
//
//  /**
//    * Retrieves a line of input from the repository.
//    */
//  final val getStrLn: ZIO[Repository, IOException, String] =
//    ZIO.accessM(_.repository.getStrLn)
}
