package part4faulttolerance

import akka.actor.{Actor, ActorLogging}

object ActorLifecycle extends App {
  class LifecycleActor extends Actor with ActorLogging {
    override def receive : Receive = {
      ???
    }
  }
}
