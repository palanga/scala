package com.howtographql.scala.sangria

import java.sql.Timestamp

import akka.http.scaladsl.model.DateTime
import com.howtographql.scala.sangria.models._
import slick.ast.BaseTypedType
import slick.jdbc.H2Profile.api._
import slick.jdbc.JdbcType

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

object DBSchema {

  class LinksTable(tag: Tag) extends Table[Link](tag, "LINKS") {

    def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)

    def url = column[String]("URL")

    def description = column[String]("DESCRIPTION")

    def postedBy = column[Int]("USER_ID")

    def postedByFK = foreignKey("postedBy_FK", postedBy, Users)(_.id)

    def createdAt = column[DateTime]("CREATED_AT")

    def * = (id, url, description, postedBy, createdAt).mapTo[Link]

  }

  class VotesTable(tag: Tag) extends Table[Vote](tag, "VOTES") {

    def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)

    def userId = column[Int]("USER_ID")

    def userFK = foreignKey("user_FK", userId, Users)(_.id)

    def linkId = column[Int]("LINK_ID")

    def linkFK = foreignKey("link_FK", linkId, Links)(_.id)

    def createdAt = column[DateTime]("CREATED_AT")

    def * = (id, userId, linkId, createdAt).mapTo[Vote]

  }

  class UsersTable(tag: Tag) extends Table[User](tag, "USERS") {

    def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)

    def name = column[String]("NAME")

    def email = column[String]("EMAIL")

    def password = column[String]("PASSWORD")

    def createdAt = column[DateTime]("CREATED_AT")

    def * = (id, name, email, password, createdAt).mapTo[User]

  }

  implicit val dateTimeColumnType: JdbcType[DateTime] with BaseTypedType[DateTime] =
    MappedColumnType.base[DateTime, Timestamp](
      dt => new Timestamp(dt.clicks),
      ts => DateTime(ts.getTime)
    )

  val Links = TableQuery[LinksTable]
  val Users = TableQuery[UsersTable]
  val Votes = TableQuery[VotesTable]

  /**
    * Load schema and populate sample data withing this Sequence od DBActions
    */
  val databaseSetup = DBIO.seq(
    Users.schema.create,
    Users forceInsertAll Seq(
      User(1, "salvador", "salvador@dali.es", "dalí", DateTime(1904, 5, 11)),
      User(2, "joan", "joan@miro.es", "miró", DateTime(1983, 12, 25)),
    ),
    Links.schema.create,
    Links forceInsertAll Seq(
      Link(1, "http://howtographql.com", "Awesome community driven GraphQL tutorial", 1),
      Link(2, "http://graphql.org", "Official GraphQL web page", 2),
      Link(3, "https://facebook.github.io/graphql/", "GraphQL specification", 2)
    ),
    Votes.schema.create,
    Votes forceInsertAll Seq(
      Vote(1, 1, 1),
      Vote(2, 2, 1),
    ),
  )

  def createDatabase: DAO = {
    val db = Database.forConfig("h2mem")

    Await.result(db.run(databaseSetup), 10 seconds)

    new DAO(db)

  }

}
