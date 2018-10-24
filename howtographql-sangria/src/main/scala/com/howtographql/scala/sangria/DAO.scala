package com.howtographql.scala.sangria

import com.howtographql.scala.sangria.DBSchema.{Links, Users, Votes}
import com.howtographql.scala.sangria.models.{AuthProviderSignupData, Link, User, Vote}
import sangria.execution.deferred.{RelationIds, SimpleRelation}
import slick.jdbc.H2Profile.api._

import scala.concurrent.Future

class DAO(db: Database) {

  def authenticate(emailAddress: String, password: String): Future[Option[User]] = db.run(
    Users.filter(user => user.email === emailAddress && user.password === password).result.headOption
  )

  def allLinks: Future[Seq[Link]] = db.run(Links.result)

  def getLinks(ids: Seq[Int]): Future[Seq[Link]] = db.run(
    Links.filter(_.id inSet ids).result
  )

  def getLinksByUserIds(ids: Seq[Int]): Future[Seq[Link]] = db.run(
    Links.filter(_.postedBy inSet ids).result
  )

  def allUsers: Future[Seq[User]] = db.run(Users.result)

  def allVotes: Future[Seq[Vote]] = db.run(Votes.result)


  def getUsers(ids: Seq[Int]): Future[Seq[User]] = db.run(
    Users.filter(_.id inSet ids).result
  )

  def getVotes(ids: Seq[Int]): Future[Seq[Vote]] = db.run(
    Votes.filter(_.id inSet ids).result
  )

  def getVotesByRelationIds(rel: RelationIds[Vote]): Future[Seq[Vote]] = db.run(
    Votes.filter { vote =>
      rel.rawIds.collect({
        case (SimpleRelation("byUser"), ids: Seq[Int]) => vote.userId inSet ids
        case (SimpleRelation("byLink"), ids: Seq[Int]) => vote.linkId inSet ids
      }).foldLeft(true: Rep[Boolean])(_ || _)
    } result
  )

  def createUser(name: String, authProvider: AuthProviderSignupData): Future[User] = {
    val newUser = User(0, name, authProvider.email.address, authProvider.email.password)

    val insertAndReturnUserQuery = (Users returning Users.map(_.id)) into {
      (user, id) => user.copy(id = id)
    }

    db.run {
      insertAndReturnUserQuery += newUser
    }

  }

  def createLink(description: String, url: String, postedById: Int): Future[Link] = {
    val insertAndReturnLinkQuery = (Links returning Links.map(_.id)) into {
      (link, id) => link.copy(id = id)
    }

    db.run {
      insertAndReturnLinkQuery += Link(0, description, url, postedById)
    }

  }

  def createVote(userId: Int, linkId: Int): Future[Vote] = {
    val insertAndReturnVoteQuery = (Votes returning Votes.map(_.id)) into {
      (vote, id) => vote.copy(id = id)
    }

    db.run {
      insertAndReturnVoteQuery += Vote(0, userId, linkId)
    }

  }

}
