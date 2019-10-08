package part3testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{CallingThreadDispatcher, TestActorRef, TestProbe}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

import scala.concurrent.duration.Duration

class SynchronousSpec extends WordSpecLike with BeforeAndAfterAll {

  implicit val system = ActorSystem("SynchronousSpec")

  override protected def afterAll(): Unit = {
    system.terminate()
  }

  import SynchronousSpec._

  "A counter" should {
    "sync increase its counter" in {
      val counter = TestActorRef[Counter](Props[Counter])
      counter ! Inc // counter has Already received the message

      assert(counter.underlyingActor.count == 1)
    }

    "sync increase its counter at the call of the receive function" in {
      val counter = TestActorRef[Counter](Props[Counter])
     counter.receive(Inc)
      assert(counter.underlyingActor.count == 1 )
    }

    "work on the calling thread dispatcher" in {
      val counter = system.actorOf(Props[Counter].withDispatcher(CallingThreadDispatcher.Id))
      val probe = TestProbe()

      probe.send(counter, Read)
      probe.expectMsg(Duration.Zero, 0) // probe have already receive the msg zero
    }
  }
}

object SynchronousSpec {
  case object Inc
  case object Read

  class Counter extends Actor {
    var count = 0

    override def receive: Receive = {
      case Inc => count += 1
      case Read => sender() ! count
    }
  }
}
