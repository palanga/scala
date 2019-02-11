package palan

import java.time.LocalDateTime

import akka.actor.ActorSystem
import akka.pattern.Patterns.after
import palan.card.immutable._

import scala.collection.immutable.Queue
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

package object CallCentre {

  implicit val ec: ExecutionContext = ExecutionContext global
  implicit val actorSystem: ActorSystem = ActorSystem()

  type AgentId = Id

  sealed abstract class Call {
    def clientId: ClientId
  }

  trait Triaged {
    def agent: Agent
  }

  case class IncomingCall(clientId: ClientId, incomingTime: LocalDateTime = LocalDateTime now) extends Call {
    def open(agent: Agent): OpenedCall = OpenedCall(this, agent)
  }

  case class OpenedCall(incomingCall: IncomingCall, agent: Agent, openTime: LocalDateTime = LocalDateTime now) extends Call with Triaged {
    override def clientId: ClientId = incomingCall clientId

    def close: ClosedCall = ClosedCall(this)
  }

  case class ClosedCall(openedCall: OpenedCall, closeTime: LocalDateTime = LocalDateTime now) extends Call with Triaged {
    override def clientId: ClientId = openedCall clientId

    override def agent: Agent = openedCall agent
  }

  case class Agent(id: AgentId) extends Identifiable {

    def answer(incomingCall: IncomingCall): Future[ClosedCall] = {
      val openedCall = incomingCall open this
      after(5 minutes, actorSystem scheduler, ec,
        Future(openedCall close)
      )
    }

  }

  case class State(busy: Set[Agent], free: Queue[Agent]) {

    val MAX_BUSY_AGENTS = 10

    def dispatch(call: Call): Call => State = Dispatcher dispatch this

    def assign(incomingCall: IncomingCall): State =
      if (busy.size == MAX_BUSY_AGENTS) throw new Exception("all busy")
      else {
        val (agent, newFree) = free dequeue

        agent answer incomingCall map dispatch

        this copy(busy + agent, newFree)
      }

    def unAssign(closedCall: ClosedCall): State = {
      val agent = closedCall agent

      this copy(busy - agent, free enqueue agent)
    }

  }

  object Dispatcher {

    def dispatch(state: State)(call: Call): State = call match {
      case c: IncomingCall => state assign c
      case c: ClosedCall => state unAssign c
      case _ => state
    }

  }

}
