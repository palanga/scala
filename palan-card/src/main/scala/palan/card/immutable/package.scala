package palan.card

import scalaz.syntax.std.map._
import scalaz.syntax.id._

package object immutable {

  type Id = Int
  type ClientId = Id
  type Amount = Int
  type Points = Int

  sealed abstract class Action
  case class Transaction(clientId: ClientId, amount: Amount) extends Action
  case class EarnPoints(clientId: ClientId, points: Points) extends Action
  case class InvalidTransaction() extends Action

  case class AtomicOperation(actions: List[Action] = Nil) {

    def ::(action: Action): AtomicOperation = this copy action :: actions

    def hasInvalid: Boolean = actions exists {
      case _: InvalidTransaction => true
      case _ => false
    }

    def sanitize: AtomicOperation = if (hasInvalid) AtomicOperation() else this

  }

  trait Identifiable {
    def id: Id
  }

  trait Client extends Identifiable {
    def buy(amount: Amount): AtomicOperation
  }

  case class BasicClient(id: ClientId) extends Client {
    override def buy(amount: Amount): AtomicOperation = Transaction(id, amount) :: Nil |> AtomicOperation
  }

  trait CommerceCondition {
    def client: Client
  }

  case class SafeShopClient(limit: Amount, client: Client) extends Client with CommerceCondition {

    override def id: ClientId = client id

    override def buy(amount: Amount): AtomicOperation =
      if (amount > limit) InvalidTransaction() :: Nil |> AtomicOperation else client buy amount

  }

  case class PromoClient(client: Client) extends Client with CommerceCondition {

    private final val MIN_AMOUNT_TO_EARN_POINTS = 50
    private final val POINTS_TO_EARN = 15

    override def id: ClientId = client id

    override def buy(amount: Amount): AtomicOperation =
      if (amount >= MIN_AMOUNT_TO_EARN_POINTS) EarnPoints(id, POINTS_TO_EARN) :: (client buy amount)
      else client buy amount

  }

  case class State(pointsPerClient: Map[ClientId, Points] = Map empty, ledger: List[Transaction] = Nil) {

    def sum(earnPoints: EarnPoints): State =
      this copy pointsPerClient.insertWith(earnPoints clientId, earnPoints points)(_ + _)

    def save(transaction: Transaction): State = this copy (ledger = transaction :: ledger)

  }

  case object Reducer {

    private def reduceSingle(state: State, action: Action): State = action match {
      case t: Transaction => state save t
      case p: EarnPoints => state sum p
      case _ => state
    }

    def reduce(state: State, atomicOperation: AtomicOperation): State =
      atomicOperation.sanitize.actions.foldLeft(state)(reduceSingle)

  }

}
