package zioborrar


import java.util.concurrent.TimeUnit
import java.time.{LocalDateTime, ZoneOffset}

import scalaz.zio.{App, ZIO, UIO}
import scalaz.zio.console.{Console, getStrLn, putStrLn}
import scalaz.zio.clock.{Clock, currentTime}
import repository.findName
import repository.findNameWithDelay
import repository.Repository
import repository.repositoryService

import scalaz.syntax.id._

object MyApp extends App {

  type CredentialId = Int
  type AuthId = String
  type AuthSecret = String

  case class AuthInfo(id: AuthId, secret: AuthSecret)

  object AuthInfo {
    val default = AuthInfo("7", "skrik")
  }


  trait LiveEnv extends Repository.Live with Console.Live with Clock.Live
  object LiveEnv extends LiveEnv


  def millisToLocalDateTimeUIO(seconds: Long): UIO[LocalDateTime] = UIO.succeed(LocalDateTime.ofEpochSecond(seconds, 0, ZoneOffset.UTC))

  def millisToLocalDateTime(seconds: Long): LocalDateTime = LocalDateTime.ofEpochSecond(seconds, 0, ZoneOffset.UTC)



  def run(args: List[String]) = stupid.provide(LiveEnv).fold(_ => 1, _ => 0)

  def stupid: ZIO[LiveEnv, Throwable, Unit] = for {
      _     <- putStrLn("choose phone number")
      id    <- getStrLn
//      name  <- findName(id)
      repo  <- repositoryService
//      name  <- repo findName id
      name  <- repo findNameWithDelay id
      _     <- putStrLn(s"Hello, $name, welcome to ZIO!")
      time  <- currentTime(TimeUnit.SECONDS)
      _     <- millisToLocalDateTime(time).toString |> putStrLn
    } yield ()

  type Request = String
  type Response = String

//  def process(request: Request): ZIO[Env, Throwable, Response] = for {
//    (uri, payload) <- parseRequest(request)
//    route          <- findRoute(uri)
//    result         <- route(payload)
//    marshaled      <- marshall(result)
//    response       <- buildResponse(marshaled)
//  } yield response

}
