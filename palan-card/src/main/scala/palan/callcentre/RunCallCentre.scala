package palan.callcentre

object RunCallCentre extends App {

//  (1 to 3).map(n => IncomingCall(PendingClient(n))).foreach(Dispatcher dispatch)

  override def main(args: Array[String]): Unit = {

//    println("lala")

//    println((1 to 3).map(n => IncomingCall(PendingClient(n))))

    val NUM_CALLS = 15

    (1 to NUM_CALLS).map(n => IncomingCall(PendingClient(n))).par.foreach(_Dispatcher dispatch)

//    return 0

  }


}
