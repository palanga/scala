package palan.callcentre

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.parallel.immutable.ParSeq

class CallCentreSuite extends FlatSpec with Matchers {

  it should "lala" in {

    val NUM_CALLS = 2000

    val calls: ParSeq[IncomingCall] = (1 to NUM_CALLS).map(n => IncomingCall(PendingClient(n))).par

//    def dispatchAndAssert(incomingCall: IncomingCall)




  }

}
