package palan.card.immutable

import org.scalatest.{FlatSpec, Matchers}
import scalaz.syntax.id._

class PalanCardImmutableSuite extends FlatSpec with Matchers {

  val CLIENT_ID = 1

  it should "let basic client buy anything" in {

    val client = BasicClient(CLIENT_ID)

    val actual = List(
      client buy 10,
      client buy 1000,
      client buy 60,
    )

    val expected = List(
      Transaction(CLIENT_ID, 10) :: Nil |> AtomicOperation,
      Transaction(CLIENT_ID, 1000) :: Nil |> AtomicOperation,
      Transaction(CLIENT_ID, 60) :: Nil |> AtomicOperation,
    )

    actual should be(expected)

  }

  it should "let promo client buy anything and sum points" in {

    val client = BasicClient(CLIENT_ID) |> PromoClient

    val actual = List(
      client buy 10,
      client buy 1000,
      client buy 60,
    )

    val expected = List(
      Transaction(CLIENT_ID, 10) :: Nil |> AtomicOperation,
      EarnPoints(CLIENT_ID, 15) :: Transaction(CLIENT_ID, 1000) :: Nil |> AtomicOperation,
      EarnPoints(CLIENT_ID, 15) :: Transaction(CLIENT_ID, 60) :: Nil |> AtomicOperation,
    )

    actual should be(expected)

  }

  it should "let safe shop client buy only below limit" in {

    val client = SafeShopClient(500, BasicClient(CLIENT_ID))

    val actual = List(
      client buy 10,
      client buy 1000,
      client buy 60,
    )

    val expected = List(
      Transaction(CLIENT_ID, 10) :: Nil |> AtomicOperation,
      InvalidTransaction() :: Nil |> AtomicOperation,
      Transaction(CLIENT_ID, 60) :: Nil |> AtomicOperation,
    )

    actual should be(expected)

  }

  it should "let safe shop promo client buy only below limit and sum points" in {

    val client = SafeShopClient(500, BasicClient(CLIENT_ID) |> PromoClient)

    val actual = List(
      client buy 10,
      client buy 1000,
      client buy 60,
    )

    val expected = List(
      Transaction(CLIENT_ID, 10) :: Nil |> AtomicOperation,
      InvalidTransaction() :: Nil |> AtomicOperation,
      EarnPoints(CLIENT_ID, 15) :: Transaction(CLIENT_ID, 60) :: Nil |> AtomicOperation,
    )

    actual should be(expected)

  }

  it should "let promo safe shop client buy only below limit and sum points" in {

    val client = SafeShopClient(500, BasicClient(CLIENT_ID)) |> PromoClient

    val actual = List(
      client buy 10,
      client buy 1000,
      client buy 60,
    )

    val expected = List(
      Transaction(CLIENT_ID, 10) :: Nil |> AtomicOperation,
      EarnPoints(CLIENT_ID, 15) :: InvalidTransaction() :: Nil |> AtomicOperation,
      EarnPoints(CLIENT_ID, 15) :: Transaction(CLIENT_ID, 60) :: Nil |> AtomicOperation,
    )

    actual should be(expected)

  }


  it should "state sum points" in {

    val expectedPoints = Map(CLIENT_ID -> 30)

    State()
      .sum(EarnPoints(CLIENT_ID, 15))
      .sum(EarnPoints(CLIENT_ID, 15)) should be(
      State(expectedPoints)
    )

  }

  it should "state save transaction" in {

    val expectedLedger = List(
      Transaction(CLIENT_ID, 770),
      Transaction(CLIENT_ID, 100),
    )

    State()
      .save(Transaction(CLIENT_ID, 100))
      .save(Transaction(CLIENT_ID, 770)) should be(
      State(ledger = expectedLedger)
    )

  }


  it should "reducer return the same state when processing invalid operations" in {

    val invalidOperations = List(
      InvalidTransaction() :: Transaction(CLIENT_ID, 1000) :: EarnPoints(CLIENT_ID, 15) :: Nil |> AtomicOperation,
      Transaction(CLIENT_ID, 1000) :: EarnPoints(CLIENT_ID, 15) :: InvalidTransaction() :: Nil |> AtomicOperation,
      EarnPoints(CLIENT_ID, 15) :: InvalidTransaction() :: Transaction(CLIENT_ID, 1000) :: Nil |> AtomicOperation,
    )

    val prevState = State(Map(CLIENT_ID -> 45), Transaction(CLIENT_ID, 200) :: Nil)

    invalidOperations foreach (Reducer.reduce(prevState, _) should be(prevState))

  }

  it should "reducer sum points" in {

    val atomicOperation = EarnPoints(CLIENT_ID, 15) :: EarnPoints(CLIENT_ID, 15) :: Nil |> AtomicOperation

    Reducer reduce(State(), atomicOperation) should be(State(Map(CLIENT_ID -> 30)))

  }

  it should "reducer add transactions" in {

    val atomicOperation = Transaction(CLIENT_ID, 1000) :: Transaction(CLIENT_ID, 770) :: Nil |> AtomicOperation
    val expectedState = State(ledger = Transaction(CLIENT_ID, 770) :: Transaction(CLIENT_ID, 1000) :: Nil)

    Reducer reduce(State(), atomicOperation) should be(expectedState)

  }

  it should "reducer add transactions and sum points" in {

    val atomicOperations = List(
      Transaction(CLIENT_ID, 320) :: EarnPoints(CLIENT_ID, 15) :: Nil |> AtomicOperation,
      EarnPoints(CLIENT_ID, 15) :: Transaction(CLIENT_ID, 320) :: Nil |> AtomicOperation,
    )

    val expectedState = State(Map(CLIENT_ID -> 15), Transaction(CLIENT_ID, 320) :: Nil)

    atomicOperations foreach (Reducer reduce(State(), _) should be(expectedState))

  }

}
