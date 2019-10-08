package part3testing

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

class TestPropSpec extends TestKit(ActorSystem("TestPropSpec"))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import TestPropSpec._


  "A master actor" should {
    "register a slave" in {
      val master = system.actorOf(Props[Master])
      val slave = TestProbe("slave")

      master ! Register(slave.ref)
      expectMsg(RegistrationAck)
    }

    "send the work to the slave actor" in {
      val master = system.actorOf(Props[Master])
      val slave = TestProbe("slave")

      master ! Register(slave.ref)
      expectMsg(RegistrationAck)

      val workloadString = "I love Akka"
      master ! Work(workloadString)

      slave.expectMsg(SlaveWork(workloadString,testActor))
      slave.reply(WorkCompleted(3,testActor))

      expectMsg(Report(3)) // test actor receives the Report(3)
    }

    "aggregate data correctly" in {
      val master = system.actorOf(Props[Master])
      val slave = TestProbe("slave")

      master ! Register(slave.ref)
      expectMsg(RegistrationAck)

      val workloadString = "I love Akka"
      master ! Work(workloadString)
      master ! Work(workloadString)

      //in the meantime i don't have a slave actor
      slave.receiveWhile() {
        case SlaveWork(`workloadString`, testActor) => slave.reply(WorkCompleted(3,testActor))
      }

      expectMsg(Report(3))
      expectMsg(Report(6))

    }
  }

}


object TestPropSpec {
  /**
    * word counting actor hoerarchy master-slave
    *
    * send some work to the master
    *   - master sends the slave the piece of work
    *   - slave processes the work and reploes to master
    *   - master aggregation the result
    *
    * master sends the total count to the original requester
    *
    */

  case class Register(slaveRef: ActorRef)
  case class Work(text: String)
  case class SlaveWork(text: String, orgRequester: ActorRef)
  case class WorkCompleted(count: Int, orgRequester: ActorRef)
  case class Report(totalCount: Int)
  case object RegistrationAck

  class Master extends Actor {
    override def receive: Receive = {
      case Register(slaveRef) =>
        sender() ! RegistrationAck
        context.become(online(slaveRef,0))
      case _ => //ignore
    }

    def online(slaveRef: ActorRef, totalWordCount: Int): Receive = {
      case Work(text) => slaveRef ! SlaveWork(text,sender())
      case WorkCompleted(count, orgRequester) =>
        val newTotalWordCount = totalWordCount + count
        orgRequester ! Report(newTotalWordCount)
        context.become(online(slaveRef,newTotalWordCount))
    }
  }


  // class Slave extends Actor ...
}