package com.howtographql.scala.sangria

import akka.http.scaladsl.model.DateTime
import sangria.execution.FieldTag
import sangria.execution.deferred.HasId
import sangria.validation.Violation

package object models {

  case object Authorized extends FieldTag

  case class AuthenticationException(message: String) extends Exception(message)

  case class AuthorizationException(message: String) extends Exception(message)

  case class AuthProviderEmail(address: String, password: String)

  case class AuthProviderSignupData(email: AuthProviderEmail)

  type Address = String
  type Name = String
  type ID = Int
  type Photo = String
  type Description = String
  type Health = String
  type Task = String

  case class CareRequest(
    id: ID,
    userId: ID,
    animalId: ID,
    careStatus: CareStatus,
    from: DateTime = DateTime.now,
    until: Option[DateTime],
    pickupAddress: List[Address] = Nil,
  ) extends Identifiable

  sealed abstract class CareStatus(val name: String)

  case object Breastfeeding extends CareStatus("Breastfeeding")

  case object Wild extends CareStatus("Wild")

  case object InTransit extends CareStatus("InTransit")

  case object Adopted extends CareStatus("Adopted")

  case object Pensioned extends CareStatus("Pensioned")

  case object InTreatment extends CareStatus("InTreatment")

  case class Animal(
    id: ID,
    name: Name,
    photos: List[Photo] = Nil,
    address: Address = "",
    description: Description = "",
    status: CareStatus = Wild,
    health: Health = "",
    tasks: List[Task] = Nil,
    careRequests: List[CareRequest] = Nil,
  ) extends Identifiable

  case class Link(
    id: Int,
    url: String,
    description: String,
    postedBy: Int,
    createdAt: DateTime = DateTime.now,
  ) extends Identifiable

  case class User(
    id: Int,
    name: String,
    email: String,
    password: String,
    createdAt: DateTime = DateTime.now,
  ) extends Identifiable

  case class Vote(id: Int, userId: Int, linkId: Int, createdAt: DateTime = DateTime.now) extends Identifiable

  trait Identifiable {
    val id: Int
  }

  object Identifiable {
    implicit def hasId[T <: Identifiable]: HasId[T, Int] = HasId(_.id)
  }

  case object DateTimeCoerceViolation extends Violation {
    override def errorMessage: String = "Error during parsing DateTime"
  }

}
