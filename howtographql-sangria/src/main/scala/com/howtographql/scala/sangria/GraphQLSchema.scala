package com.howtographql.scala.sangria

import akka.http.scaladsl.model.DateTime
import com.howtographql.scala.sangria.models._
import sangria.ast.StringValue
import sangria.execution.deferred._
import sangria.macros.derive.{AddFields, InputObjectTypeName, ReplaceField, deriveInputObjectType, deriveObjectType}
import sangria.schema._
import sangria.marshalling.sprayJson._
import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

object Types {

  import GraphQLSchema._

  implicit val AuthProviderEmailInputType: InputObjectType[AuthProviderEmail] = deriveInputObjectType(
    InputObjectTypeName("AUTH_PROVIDER_EMAIL")
  )

  lazy val AuthProviderSignupDataInputType: InputObjectType[AuthProviderSignupData] = deriveInputObjectType()

  implicit val authProviderEmailFormat: RootJsonFormat[AuthProviderEmail] = jsonFormat2(AuthProviderEmail)

  implicit val authProviderSignupDataFormat: RootJsonFormat[AuthProviderSignupData] = jsonFormat1(
    AuthProviderSignupData
  )

  lazy val UserType: ObjectType[Unit, User] = deriveObjectType(
    AddFields(
      Field("links", ListType(LinkType), resolve = c => linksFetcher.deferRelSeq(linkByUserRel, c.value.id)),
      Field("votes", ListType(VoteType), resolve = c => votesFetcher.deferRelSeq(voteByUserRel, c.value.id)),
    )
  )

  lazy val LinkType: ObjectType[Unit, Link] = deriveObjectType(
    ReplaceField("postedBy",
      Field("postedBy", UserType, resolve = c => usersFetcher.defer(c.value.postedBy))
    ),
    AddFields(
      Field("votes",
        ListType(VoteType), resolve = c => votesFetcher.deferRelSeq(voteByLinkRel, c.value.id)
      )
    )
  )

  implicit val VoteType: ObjectType[Unit, Vote] = deriveObjectType(
    ReplaceField("userId",
      Field("user", UserType, resolve = c => usersFetcher.defer(c.value.userId))
    ),
    ReplaceField("linkId",
      Field("link", LinkType, resolve = c => linksFetcher.defer(c.value.linkId))
    ),
  )

  implicit val GraphQLDateTime: ScalarType[DateTime] = ScalarType(
    "DateTime",
    coerceOutput = (dt, _) => dt.toString(),
    coerceInput = {
      case StringValue(dt, _, _) => DateTime.fromIsoDateTimeString(dt).toRight(DateTimeCoerceViolation)
      case _ => Left(DateTimeCoerceViolation)
    },
    coerceUserInput = {
      case s: String => DateTime.fromIsoDateTimeString(s).toRight(DateTimeCoerceViolation)
      case _ => Left(DateTimeCoerceViolation)
    }
  )

}

object Fields {

  import GraphQLSchema._
  import Types._

  val Id = Argument("id", IntType)
  val Ids = Argument("ids", ListInputType(IntType))

  val NameArg = Argument("name", StringType)
  val AuthProviderArg = Argument("authProvider", AuthProviderSignupDataInputType)

  val DescriptionArg = Argument("description", StringType)
  val UrlArg = Argument("url", StringType)
  val PostedByIdArg = Argument("postedById", IntType)

  val UserIdArg = Argument("userId", IntType)
  val LinkIdArg = Argument("linkId", IntType)

  val EmailAddressArg = Argument("emailAddress", StringType)
  val PasswordArg = Argument("password", StringType)

  val linkFields: List[Field[MyContext, Unit]] = fields[MyContext, Unit](
    Field("allLinks", ListType(LinkType), resolve = _.ctx.dao.allLinks),
    Field("link",
      OptionType(LinkType),
      arguments = Id :: Nil,
      resolve = c => linksFetcher.defer(c.arg(Id)),
    ),
    Field("links",
      ListType(LinkType),
      arguments = List(Argument("ids", ListInputType(IntType))),
      resolve = c => linksFetcher.deferSeq(c.arg(Ids))
    ),
    Field("user",
      OptionType(UserType),
      arguments = Id :: Nil,
      resolve = c => usersFetcher.defer(c.arg(Id))
    ),
    Field("users",
      ListType(UserType),
      arguments = List(Argument("ids", ListInputType(IntType))),
      resolve = c => usersFetcher.deferSeq(c.arg(Ids))
    ),
    Field("vote",
      OptionType(VoteType),
      arguments = Id :: Nil,
      resolve = c => votesFetcher.defer(c.arg(Id))
    ),
    Field("votes",
      ListType(VoteType),
      arguments = List(Argument("ids", ListInputType(IntType))),
      resolve = c => votesFetcher.deferSeq(c.arg(Ids))
    ),
    Field("allUsers", ListType(UserType), resolve = _.ctx.dao.allUsers),
    Field("allVotes", ListType(VoteType), resolve = _.ctx.dao.allVotes),
  )

}

object GraphQLSchema {

  import Types._
  import Fields._

  val linkByUserRel: Relation[Link, Link, Int] = Relation("byUser", l => Seq(l.postedBy))

  val voteByUserRel: Relation[Vote, Vote, Int] = Relation("byUser", v => Seq(v.userId))

  val voteByLinkRel: Relation[Vote, Vote, Int] = Relation("byLink", v => Seq(v.linkId))

  val linksFetcher: Fetcher[MyContext, Link, Link, Int] = Fetcher.rel(
    (ctx: MyContext, ids: Seq[Int]) => ctx.dao.getLinks(ids),
    (ctx: MyContext, ids: RelationIds[Link]) => ctx.dao.getLinksByUserIds(ids(linkByUserRel)),
  )

  val usersFetcher = Fetcher(
    (ctx: MyContext, ids: Seq[Int]) => ctx.dao.getUsers(ids),
  )

  val votesFetcher: Fetcher[MyContext, Vote, Vote, Int] = Fetcher.rel(
    (ctx: MyContext, ids: Seq[Int]) => ctx.dao.getVotes(ids),
    (ctx: MyContext, ids: RelationIds[Vote]) => ctx.dao.getVotesByRelationIds(ids),
  )

  val Resolver: DeferredResolver[MyContext] = DeferredResolver.fetchers(linksFetcher, usersFetcher, votesFetcher)

  val QueryType = ObjectType("Query", Fields.linkFields)

  val Mutation = ObjectType(
    "Mutation",
    fields[MyContext, Unit](
      Field("createUser",
        UserType,
        arguments = NameArg :: AuthProviderArg :: Nil,
        resolve = c => c.ctx.dao.createUser(c.arg(NameArg), c.arg(AuthProviderArg))
      ),
      Field("createLink",
        LinkType,
        arguments = DescriptionArg :: UrlArg :: PostedByIdArg :: Nil,
        resolve = c => c.ctx.dao.createLink(c.arg(DescriptionArg), c.arg(UrlArg), c.arg(PostedByIdArg)),
        tags = Authorized :: Nil,
      ),
      Field("createVote",
        VoteType,
        arguments = UserIdArg :: LinkIdArg :: Nil,
        resolve = c => c.ctx.dao.createVote(c.arg(UserIdArg), c.arg(LinkIdArg))
      ),
      Field("login",
        UserType,
        arguments = EmailAddressArg :: PasswordArg :: Nil,
        resolve = c => UpdateCtx(c.ctx.login(c.arg(EmailAddressArg), c.arg(PasswordArg))) { user =>
          c.ctx.copy(currentUser = Some(user))
        }
      ),
    )
  )

  val SchemaDefinition = Schema(QueryType, Some(Mutation))

}
