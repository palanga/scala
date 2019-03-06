package palan

import java.time.LocalDateTime

import scala.collection.mutable

//import akka.dispatch.Dispatcher
import akka.actor.ActorSystem
import akka.pattern.Patterns.after
import palan.card.immutable._

import scala.collection.immutable.Queue
import scala.collection.mutable.{Queue => MutableQueue}
import scala.collection.mutable.{Set => MutableSet}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

package object callcentre {



  val MAX_BUSY_AGENTS = 3
  val NUM_AGENTS = 5

//  (1 to 3).map(n => IncomingCall(PendingClient(n))).foreach(Dispatcher dispatch)

  implicit val executionContext: ExecutionContext = ExecutionContext global
  implicit val actorSystem: ActorSystem = ActorSystem()

  type AgentId = Id

  trait Client extends Identifiable

  case class PendingClient(id: ClientId) extends Client {
    def activate(openedCall: OpenedCall): ActiveClient = ActiveClient(id, openedCall)
  }

  case class ActiveClient(id: ClientId, openedCall: OpenedCall) extends Client {
//    def speak: Future[SolvedCall] = Future(openedCall solve)

    val random = Math.random * 7000

    def speak: Future[SolvedCall] = after(random millis, actorSystem scheduler, executionContext, Future(openedCall solve))
  }

  sealed abstract class Call {
    def client: Client
  }

  trait Triaged {
    def agent: Agent
    def client: ActiveClient
  }

  case class IncomingCall(client: PendingClient, incomingTime: LocalDateTime = LocalDateTime now) extends Call {
    def open(agent: Agent, dateTime: => LocalDateTime): OpenedCall = OpenedCall(this, agent, dateTime)
  }

  case class OpenedCall(incomingCall: IncomingCall, agent: Agent, openedTime: LocalDateTime) extends Call with Triaged {

    override def client: ActiveClient = incomingCall.client activate this

    def close(dateTime: => LocalDateTime): ClosedCall = ClosedCall(this, dateTime)

    def solve: SolvedCall = SolvedCall(this)

  }

  case class SolvedCall(openedCall: OpenedCall) extends Call with Triaged {

    override def client: ActiveClient = openedCall client

    override def agent: Agent = openedCall agent

    def close(dateTime: => LocalDateTime): ClosedCall = ClosedCall(this, dateTime)

  }

  case class ClosedCall(openedCall: Call with Triaged, closedTime: LocalDateTime) extends Call with Triaged {

    override def client: ActiveClient = openedCall client

    override def agent: Agent = openedCall agent

  }

  case class Agent(id: AgentId) extends Identifiable {

    def answer(incomingCall: IncomingCall): Future[ClosedCall] =
      incomingCall.open(this, LocalDateTime now).client.speak.map(_ close (LocalDateTime now))

  }

  case class State(
    free: MutableQueue[Agent],
    busy: MutableSet[Agent] = MutableSet empty,
    unhandledCalls: MutableQueue[IncomingCall] = MutableQueue empty
  ) {

//    val MAX_BUSY_AGENTS = 10

    def assign(incomingCall: IncomingCall): State = this synchronized {
      if (busy.size == MAX_BUSY_AGENTS) {
        unhandledCalls enqueue incomingCall

        printThisWith(incomingCall)

        this
      }
      else {
        val agent = free dequeue

        agent answer incomingCall map _Dispatcher.dispatch

        busy += agent

        printThisWith(incomingCall)

        this
      }
    }

    def unAssign(closedCall: ClosedCall): State = this synchronized {
      val agent = closedCall agent

      free enqueue agent
      busy -= agent

      if (unhandledCalls nonEmpty) _Dispatcher dispatch (unhandledCalls dequeue)

      printThisWith(closedCall)

      this
    }

    def printThisWith(call: Call): Unit = println(
      s"""
        |==============================================
        |free: ${free.map(f => s"${f.id}").mkString(" ")}
        |busy: ${busy.map(b => s"${b.id}").mkString(" ")}
        |unhandledCalls: ${unhandledCalls.map(c => s"${c.client.id}").mkString(" ")}
        |call: $call
        |==============================================
      """.stripMargin
    )

  }

  object _Dispatcher {

    val state: State = State({
      val mutableQueue: MutableQueue[Agent] = MutableQueue empty

      (1 to NUM_AGENTS map Agent).foreach(a => mutableQueue.enqueue(a))

      mutableQueue
    })
//    val state: State = State((1 to NUM_AGENTS map Agent).foldLeft(Queue[Agent]())(_ enqueue _))

//    val execute: Runnable => Unit = actorSystem.dispatcher.execute

    def dispatch(call: Call): State = {

//      println("////////////////////////////////////////////////////////")
//      println()
//      println(s"prevState: $state")
//      println()
//      println(s"call: $call")
//      println()

      call match {
        case c: IncomingCall => state assign c
        case c: ClosedCall => state unAssign c
        case _ => state
      }

//      println(s"newState: $state")
//      println()
//      println("////////////////////////////////////////////////////////")
//
//      state

    }

  }





//  val dispatcher = new Dispatcher()






}
