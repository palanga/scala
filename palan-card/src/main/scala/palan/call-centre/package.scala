package palan

import java.time.LocalDateTime

import akka.actor.ActorSystem
import akka.pattern.Patterns.after
import palan.card.immutable._

import scala.collection.immutable.Queue
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

package object CallCentre {

  implicit val executionContext: ExecutionContext = ExecutionContext global
  implicit val actorSystem: ActorSystem = ActorSystem()

  type AgentId = Id

  sealed abstract class Call {
    def client: Client
  }

  case class Client(id: ClientId) extends Identifiable {
    def speak: Future[Unit] = after(7 seconds, actorSystem scheduler, executionContext, Future())
  }

  trait Triaged {
    def agent: Agent
  }

  case class IncomingCall(client: Client, incomingTime: LocalDateTime = LocalDateTime now) extends Call {
    def open(agent: Agent, dateTime: => LocalDateTime): OpenedCall = OpenedCall(this, agent, dateTime)
  }

  case class OpenedCall(incomingCall: IncomingCall, agent: Agent, openTime: LocalDateTime) extends Call with Triaged {
    override def client: Client = incomingCall client

    def close(dateTime: => LocalDateTime): ClosedCall = ClosedCall(this, dateTime)
  }

  case class ClosedCall(openedCall: OpenedCall, closeTime: LocalDateTime) extends Call with Triaged {
    override def client: Client = openedCall client

    override def agent: Agent = openedCall agent
  }

  case class Agent(id: AgentId) extends Identifiable {

    def answer(incomingCall: IncomingCall): Future[ClosedCall] = {

      val openedCall = incomingCall.open(this, LocalDateTime now)

      for {
        _ <- openedCall.client.speak
      } yield openedCall.close(LocalDateTime.now)

    }

  }

  case class State(free: Queue[Agent], busy: Set[Agent] = Set empty) {

    val MAX_BUSY_AGENTS = 10

    def assign(incomingCall: IncomingCall): State =
      if (busy.size == MAX_BUSY_AGENTS) throw new Exception("all busy")
      else {
        val (agent, newFree) = free dequeue

        agent answer incomingCall map Dispatcher.dispatch

        this copy(newFree, busy + agent)
      }

    def unAssign(closedCall: ClosedCall): State = {
      val agent = closedCall agent

      this copy(free enqueue agent, busy - agent)
    }

  }

  object Dispatcher {

    var state = State((1 to 30 map Agent).foldLeft(Queue[Agent]())(_ enqueue _))

    def dispatch(call: Call): Unit = {

      println()
      println("////////////////////////////////////////////////////////")
      println()
      println(s"prevState: $state")
      println()
      println(s"call: $call")
      println()

      call match {
        case c: IncomingCall => state = state assign c
        case c: ClosedCall => state = state unAssign c
        case _ =>
      }

      println(s"newState: $state")
      println()
      println("////////////////////////////////////////////////////////")


    }

  }

}
