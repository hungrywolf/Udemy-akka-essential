package part3testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

import scala.concurrent.duration._
import scala.util.Random

class BasicSpec extends TestKit(ActorSystem("BasicSpec"))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll {

  // setup
  override def afterAll():Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import BasicSpec._

  "A simple actor" should {
    "send back the same message" in {
      val echoActor = system.actorOf(Props[SimpleActor])
      val message = "hello, test"
      echoActor ! message

      expectMsg(message)
    }
  }

  "A blackhole actor" should {
    "send back the same message" in {
      val blackhole = system.actorOf(Props[Blackhole])
      val message = "hello, test"
      blackhole ! message

      expectNoMessage(1 second)
    }
  }

  //message assertion
  "a labtest actor" should {

    val labTestActor = system.actorOf(Props[LabTestActor])

    "turn a string to upper case" in {
      labTestActor ! "i love akka"
//      expectMsg("I LOVE AKKA")
      val reply = expectMsgType[String]

      assert(reply == "I LOVE AKKA")
    }

    "reply to a greeting" in {
      labTestActor ! "greeting"
      expectMsgAnyOf("hi", "hello")
    }

    "reply with fav tech" in {
      labTestActor ! "favTech"
      expectMsgAllOf("Scala","Akka")
    }

    "reply with cool tech in a diff way" in {
      labTestActor ! "favTech"
      val messages = receiveN(2) //Seq[Any]

      // free to do more compilcated assertions
    }

    "reply with cool tech in a fancy way" in {
      labTestActor ! "favTech"

      expectMsgPF() {
        case "Scala" => //only care that the PF defined
        case "Akka" =>
      }
    }
  }

}

object BasicSpec {
  class SimpleActor extends Actor {
    override def receive: Receive = {
      case message => sender() ! message
    }
  }

  class Blackhole extends Actor {
    override def receive: Receive = Actor.emptyBehavior
  }

  class LabTestActor extends Actor {
    val random = new Random()
    override def receive: Receive = {
      case "greeting" =>
        if(random.nextBoolean()) sender() ! "hi" else sender() ! "hello"
      case "favTech" =>
        sender() ! "Scala"
        sender() ! "Akka"
      case message: String => sender() ! message.toUpperCase
    }
  }
}
