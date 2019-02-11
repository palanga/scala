package palan.card

import scala.collection.mutable.{Map, MutableList}

package object mutable {


  type Amount = Int
  type Transaction = (Id, Amount)
  type Id = Int

  trait Identifiable {
    def id: Id
  }

  trait PointsAccumulable {
    def accumulatedPoints: Int

    def sumPoints: Unit
  }

  trait Client extends Identifiable {
    def buy(amount: Amount): Unit
  }

  case class Ledger() {

    val transactions: MutableList[Transaction] = MutableList empty

    def save(transaction: Transaction): Unit = transactions += transaction

  }

  case class PointsRepository() {

    val repository: Map[Id, Int] = Map.empty

    def save(client: Client with PointsAccumulable): Unit = repository += (client.id -> client.accumulatedPoints)

  }

  case class BasicClient(id: Id)(implicit ledger: Ledger) extends Client {
    override def buy(amount: Amount): Unit = {
      ledger save(this id, amount)
    }
  }

  abstract class ClientWithComercialCondition(client: Client) extends Client

  case class SafeShopClient(client: Client, limit: Amount) extends ClientWithComercialCondition(client) {

    override def id: Id = client id

    override def buy(amount: Amount): Unit = {
      if (amount > limit) throw new Exception(s"Trying to buy $amount when limit is $limit")
      else client buy amount
    }

  }

  case class PromoClient(client: Client, var accumulatedPoints: Int = 0)(implicit pointsRepository: PointsRepository)
    extends ClientWithComercialCondition(client) with PointsAccumulable {

    override def id: Id = client id

    private val MINIMUM_AMOUNT = 50
    private val POINTS = 15

    override def buy(amount: Amount): Unit = {
      client buy amount
      if (amount >= MINIMUM_AMOUNT) {
        sumPoints
        pointsRepository save this
      }
    }

    override def sumPoints: Unit = accumulatedPoints += POINTS

  }

}
