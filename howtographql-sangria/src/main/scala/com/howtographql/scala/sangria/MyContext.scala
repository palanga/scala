package com.howtographql.scala.sangria

import com.howtographql.scala.sangria.models.{AuthenticationException, AuthorizationException, User}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

case class MyContext(dao: DAO, currentUser: Option[User] = None) {

  // TODO refactor this to not use Await or Duration.Inf
  def login(emailAddress: String, password: String): User = {
    val userOpt = Await.result(dao.authenticate(emailAddress, password), Duration.Inf)
    userOpt.getOrElse(throw AuthenticationException("Incorrect email or password."))
  }

  def ensureAuthenticated(): Unit = if (currentUser.isEmpty)
    throw AuthorizationException("You do not have permission. Please sign in.")

}
