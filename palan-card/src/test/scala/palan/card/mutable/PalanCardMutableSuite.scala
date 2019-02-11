package palan.card.mutable

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.{Map, MutableList}

class PalanCardMutableSuite extends FlatSpec with Matchers {

  it should "let basic client buy anything" in {

    implicit val ledger = Ledger()

    val client = BasicClient(1)

    client buy 40
    client buy 160
    client buy 4700
    client buy 7
    client buy 320

    val expectedTransactions = MutableList(
      client.id -> 40,
      client.id -> 160,
      client.id -> 4700,
      client.id -> 7,
      client.id -> 320,
    )

    ledger.transactions should be(expectedTransactions)

  }

  it should "sum 45 points" in {

    implicit val ledger = Ledger()
    implicit val pointsRepository = PointsRepository()

    val CLIENT_ID = 1

    val promoClient = PromoClient(BasicClient(CLIENT_ID))

    promoClient buy 40
    promoClient buy 160
    promoClient buy 4700
    promoClient buy 7
    promoClient buy 320

    val expectedTransactions: MutableList[Transaction] = MutableList(
      CLIENT_ID -> 40,
      CLIENT_ID -> 160,
      CLIENT_ID -> 4700,
      CLIENT_ID -> 7,
      CLIENT_ID -> 320,
    )

    val expectedPointsRepository: Map[Id, Int] = Map(promoClient.id -> promoClient.accumulatedPoints)

    ledger.transactions should be(expectedTransactions)
    pointsRepository.repository should be(expectedPointsRepository)

  }

  it should "let safe shop client buy only below 1000" in {

    implicit val ledger = Ledger()

    val CLIENT_ID = 1

    val safeShopClient = SafeShopClient(BasicClient(CLIENT_ID), 1000)

    safeShopClient buy 45
    safeShopClient buy 160

    intercept[Exception](safeShopClient buy 4700)

    safeShopClient buy 7
    safeShopClient buy 320

    val expectedTransactions = MutableList(
      CLIENT_ID -> 45,
      CLIENT_ID -> 160,
      CLIENT_ID -> 7,
      CLIENT_ID -> 320,
    )

    ledger.transactions should be(expectedTransactions)

  }

  it should "let promo safe shop client buy only below 1000 and sum 30 points" in {

    implicit val ledger = Ledger()
    implicit val pointsRepository = PointsRepository()

    val CLIENT_ID = 1

    val promoSafeShopClient = PromoClient(SafeShopClient(BasicClient(CLIENT_ID), 1000))

    promoSafeShopClient buy 40
    promoSafeShopClient buy 160

    intercept[Exception](promoSafeShopClient buy 4700)

    promoSafeShopClient buy 7
    promoSafeShopClient buy 320

    val expectedTransactions = MutableList(
      CLIENT_ID -> 40,
      CLIENT_ID -> 160,
      CLIENT_ID -> 7,
      CLIENT_ID -> 320,
    )

    val expectedPointsRepository: Map[Id, Int] = Map(CLIENT_ID -> 30)

    ledger.transactions should be(expectedTransactions)
    pointsRepository.repository should be(expectedPointsRepository)

  }


  it should "let safe shop promo client buy only below 1000 and sum 30 points" in {

    implicit val ledger = Ledger()
    implicit val pointsRepository = PointsRepository()

    val CLIENT_ID = 1

    // TODO esto no me gusta
    // La unica forma de obtener los puntos del promo client es guardandome una referencia a el.
    // Obviamente tambien puedo averiguarlo desde el pointsRepo, pero ese no es el punto. El punto es que
    // un SafeShopClient de PromoClient no tiene el metodo accumulatedPoints.
    // Igualmente, al final nunca uso esta referencia.
    val promoClient = PromoClient(BasicClient(CLIENT_ID))
    val safeShopPromoClient = SafeShopClient(promoClient, 1000)

    safeShopPromoClient buy 40
    safeShopPromoClient buy 160

    intercept[Exception](safeShopPromoClient buy 4700)

    safeShopPromoClient buy 7
    safeShopPromoClient buy 320

    val expectedTransactions = MutableList(
      CLIENT_ID -> 40,
      CLIENT_ID -> 160,
      CLIENT_ID -> 7,
      CLIENT_ID -> 320,
    )

    val expectedPointsRepository: Map[Id, Int] = Map(CLIENT_ID -> 30)

    ledger.transactions should be(expectedTransactions)
    pointsRepository.repository should be(expectedPointsRepository)

  }

}
