package part2actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.event.Logging

object ActorLoggingDemo extends App {

  class SimpleActorWithExplicitLogger extends Actor {
    // #1 - explicit logging
    val logger = Logging(context.system, this)

    override def receive: Receive = {
    /**
      * DEBUG
      * INFO
      * ERROR
      * WARN
      *
      */
      case message => logger.info(message.toString)
    }
  }


  val system = ActorSystem("simpleActorWithExplicitLogger")
  val loggingActor = system.actorOf(Props[SimpleActorWithExplicitLogger])

  loggingActor ! "Test logging Explicit"


  // #2 ActorLogging
  class ActorWithLogging extends Actor with ActorLogging {
    override def receive: Receive = {
      case (a,b) => log.info("two things: {} and {}", a, b)
      case messgae => log.info(messgae.toString)
    }
  }

  val actorWithLogging = system.actorOf(Props[ActorWithLogging])
  actorWithLogging ! "Logging a simple message by extending trait"
  actorWithLogging ! (42,65)

}
