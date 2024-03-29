package part3testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

import scala.concurrent.duration._
import scala.util.Random

class TimedAssertionSpec extends TestKit(
  ActorSystem("TimedAssertionSpec",ConfigFactory.load().getConfig("speicalTimesAssersiotnConfig")))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import TimedAssertionSpec._

  "A Worker Actor" should {
    val workerActor = system.actorOf(Props[WorkerActor])

    "reply with the meaning of like in timely manner" in {
      within(500 millis, 1 second) {
        workerActor ! "work"
        expectMsg(WorkResult(42))
      }
    }

    "rely with valid work at the reasnable candece" in {
      within(1 second) {
        workerActor ! "workSequence"

        val results : Seq[Int] = receiveWhile[Int](max=10 seconds, idle=500 millis, messages=10) {
          case WorkResult(result) => result
        }

        assert(results.sum > 5)
      }
    }

    "reply with a test probe in a timely manner" in {
      within(1 second) {
        val probe = TestProbe()
        probe.send(workerActor, "work")
        probe.expectMsg(WorkResult(42)) // timeout in 0.3s
      }
    }
  }
}


object TimedAssertionSpec {

  case class WorkResult(i: Int)

  class WorkerActor extends Actor {
    override def receive: Receive = {
      case "work" =>
        Thread.sleep(500)
        sender() ! WorkResult(42)
      case "workSequence" =>
        val r = new Random
        for(i <- 1 to 10 ) {
          Thread.sleep(r.nextInt(50))
          sender() ! WorkResult(1)
        }
    }
  }
}