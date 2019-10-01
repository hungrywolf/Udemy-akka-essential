package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ChangingActorBehavior.Mom.MomStart

object ChangingActorBehavior extends App {

  object FussyKid {
    case object KidAccept
    case object KidReject
    val HAPPY = "happy"
    val SAD = "sad"
  }

  class FussyKid extends Actor {
    import FussyKid._
    import Mom._

    var state = HAPPY
    override def receive: Receive = {
      case Food(VEGETABLE) => state=SAD
      case Food(CHOCOLATE) => state=HAPPY
      case Ask(_) =>
        if(state == HAPPY) sender() ! KidAccept
        else sender() ! KidReject

    }
  }

  class StatelessFussyKid extends Actor {
    import FussyKid._
    import Mom._

    override def receive: Receive = happyReceive

    def happyReceive: Receive = {
      case Food(VEGETABLE) => context.become(sadReceive, false) //change my receive handler to sadRecevie
      case Food(CHOCOLATE) => // stay happy
      case Ask(_) => sender() ! KidAccept
    }

    def sadReceive: Receive = {
      case Food(VEGETABLE) =>  context.become(sadReceive, false)
      case Food(CHOCOLATE) => context.unbecome() //change my state to happy
      case Ask(_) => sender() ! KidReject
    }

  }

  object Mom {
    case class MomStart(kid: ActorRef)
    case class Food(food: String)
    case class Ask(message: String)
    val VEGETABLE = "veggies"
    val CHOCOLATE = "chocolate"
  }

  class Mom extends Actor {
    import Mom._
    import FussyKid._

    override def receive: Receive = {
      case MomStart(kid) =>
        kid ! Food(VEGETABLE)
        kid ! Food(VEGETABLE)
        kid ! Food(CHOCOLATE)
        kid ! Food(CHOCOLATE)
        kid ! Ask("don you want to play?")
      case KidAccept => println("Yay, My kid is happy!")
      case KidReject => println("My kid is sad! , but he is healthy!")
    }
  }

  val system = ActorSystem("changingActorBehavior")
  val fussyKid = system.actorOf(Props[FussyKid])
  val statelessFussyKid = system.actorOf(Props[StatelessFussyKid])
  val mom = system.actorOf(Props[Mom])

  mom ! MomStart(statelessFussyKid)

  /***
    * mom receive MomStart
    *   kid receives Food(VEGETABLE) -> kid will chnage the handler to sadReceive
    *   kid receives Ask(Play?) -> kid replies with the sadReceive handler => mom receive KidReject
    *
    */

  /**
    * @@@ context become when using false
    * Food(VEGETABLE) -> stack.push(sadReceive)
    * Food(CHOCOLATE) -> stack.push(happyReceive)
    *
    * Stack :
    * 1. happyReceive
    * 2. sadReceive
    * 3. happyReceive
    *
    */

  /***
    * @@@ new behavior with unbecome and
    * Food(VEGETABLE)
    * Food(VEGETABLE)
    * Food(CHOCOLATE)
    *
    * Stack :
    * 1. sadReceive //pop out using unbecome
    * 2. sadReceive
    * 3. happyReceive
    *
    */

  /**
    * Ex :-
    *
    * 1 - recreate the counter Actor ith context.become and no MUTABLE state
    */

  object Counter {
    case object Increment
    case object Decrement
    case object Print
  }

  class Counter extends Actor {
    import Counter._

    override def receive: Receive = countReceive(0)

    def countReceive(currentCount: Int):Receive = {
      case Increment =>
        println(s"[$currentCount] incrementing")
        context.become(countReceive(currentCount + 1))
      case Decrement =>
        println(s"[$currentCount] decrementing")
        context.become(countReceive(currentCount - 1))
      case Print => println(s"[counter] my current count is $currentCount")
    }
  }
  import Counter._
  val counter = system.actorOf(Props[Counter],"myCounter")
  (1 to 5 ).foreach(_ => counter ! Increment)
  (1 to 3 ).foreach(_ => counter ! Decrement)

  counter ! Print

  /***
    * Excercise 2 - simplified voting system
    */

  object Citizen {
    case class Vote(candidate:String)
    case object VoteStatusRequest
    case class VoteStatusReply(candidate: Option[String])
  }

  class Citizen extends Actor {
    import Citizen._

    override def receive: Receive = {
      case Vote(c) => context.become(voted(c))
      case VoteStatusRequest => sender() ! VoteStatusReply(None)
    }

    def voted(candidate: String) :Receive = {
      case VoteStatusRequest => sender() ! VoteStatusReply(Some(candidate))
    }
  }

  object VoterAggregator {
    case class AggregateVoters(citizens: Set[ActorRef])
  }

  class VoterAggregator extends Actor {
    import VoterAggregator._
    import Citizen._

    override def receive: Receive = awitingCommand

    def awitingCommand: Receive = {
      case AggregateVoters(citizens) =>
        citizens.foreach( citizensRef => citizensRef ! VoteStatusRequest)
        context.become(awitingStatuses(citizens,Map()))
    }

    def awitingStatuses(stillWaiting: Set[ActorRef] ,currentStatus: Map[String, Int]) : Receive = {
      case VoteStatusReply(None) => sender() ! VoteStatusRequest
      case VoteStatusReply(Some(candidate)) =>
        val newStillWaiting = stillWaiting - sender()
        val currentVoterCandidate = currentStatus.getOrElse(candidate,0)
        val newStats = currentStatus + (candidate -> (currentVoterCandidate +1 ))

        if(newStillWaiting.isEmpty) {
          println(s"[aggregator poll stats: $newStats")
        } else {
          context.become(awitingStatuses(newStillWaiting, newStats))
        }
    }
  }

  import VoterAggregator._
  import Citizen._

  val alice = system.actorOf(Props[Citizen])
  val bob = system.actorOf(Props[Citizen])
  val charlie = system.actorOf(Props[Citizen])
  val daniel = system.actorOf(Props[Citizen])

  alice ! Vote("Martin")
  bob ! Vote("Jonas")
  charlie ! Vote("Roland")
  daniel ! Vote("Roland")

  val voterAggregator = system.actorOf(Props[VoterAggregator])
  voterAggregator ! AggregateVoters(Set(alice,bob,charlie,daniel))

  /**
    * Print the status of the votes
    *
    * Martin -> 1
    * Jones -> 1
    * Roland -> 2
    *
    */


    
}
