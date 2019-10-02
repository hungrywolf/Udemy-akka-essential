package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChildActorExercise extends App {

  // Distributed word counting
  object WordCounterMaster {
    case class Initialize(nChildren :Int)
    case class WordCountTask(Id:Int,text:String)
    case class WordCountReply(Id:Int,count:Int)
  }
  class WordCounterMaster extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case Initialize(nChildren) =>
        println(s"[master] initialising ... ")
        val childrenRefs = for(i  <- 1 to nChildren)
          yield context.actorOf(Props[WordCounterWorker],s"wcw_$i")
        context.become(withChildern(childrenRefs, 0, 0, Map()))

    }

    def withChildern(refs: Seq[ActorRef], currentChildIndex: Int, currentTaskId: Int, requestMap: Map[Int, ActorRef]) : Receive = {
      case text: String =>
        println(s"[master] I received: $text - i will send it to child $currentChildIndex")
        val originalSender = sender()
        val task = WordCountTask(currentTaskId,text)
        val childRef = refs(currentChildIndex)
        childRef ! task
        val nextChildIndex = (currentChildIndex + 1) % refs.length
        val newTaskId = currentTaskId + 1
        val newRequestMAp = requestMap + (currentTaskId -> originalSender)
        context.become(withChildern(refs, nextChildIndex, newTaskId, newRequestMAp))

      case WordCountReply(id, count) =>
        println(s"[master] I received a reply for task id $id with $count")
        val originalSender = requestMap(id)
        originalSender ! count
        context.become(withChildern(refs, currentChildIndex, currentTaskId, requestMap - id))
    }
  }

  class WordCounterWorker extends Actor {
    import WordCounterMaster._
    override def receive: Receive = {
      case WordCountTask(id,text) =>
        println(s"[${self.path}] I have received a task $id with $text")
        val count = text.split(" ").length
        sender() ! WordCountReply(id,count)
    }
  }

  class TestActor extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case "go" =>
        val master = context.actorOf(Props[WordCounterMaster],"master")
        master ! Initialize(3)
        val texts = List("I love Akka", "Scala is Super dope", "yes","me too")
        texts.foreach(text => master ! text)
      case count:Int =>
        println(s"[test actor] i received a reply: $count")
    }
  }

  val system = ActorSystem("wordCount")
  val testActor = system.actorOf(Props[TestActor])

  testActor ! "go"

  /**
    * create WordCounteMaster
    * send an Initialize(10) to WordCounterMaster
    * send "Akka is awesome" to woredcountermaster
    * wcm will send a WordCountTask(...) to the master
    * master replies with 3 to the sender
    *
    * requester -> wcm -> wcm
    *         r <- wcm <-
    */
    // round robin logic
    // 1,2,3,4,5 and 7 tasks
    // 1,2,3,4,5,1,2

}
