package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorCapabilities extends App {

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case "Hi" => context.sender()  /*sender()*/ ! "Hello , there!"
      case message: String => println(s"[${context.self.path}] i have received $message")
      case number: Int => println(s"[$self] i have received a NUMBER $number")
      case SpacialMessage(contents) => println(s"[simple actor] i have received somthign SPEACIL :  $contents")
      case SendMessageToYourSelf(contents) => self ! contents
      case SayHiTo(ref) => ref ! "Hi"
      case WirelessPhoneMessage(content, ref) => ref forward (content+"s") // i keep the original content
    }
  }

  val system = ActorSystem("actorCapabilities")
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")

  simpleActor ! "hello, actor"

  // 1 - messages can be of any type
  // a) msg must be IMMUTABLE
  // b) msg must be SERIALIZABLE

  simpleActor ! 42

  case class SpacialMessage(contents: String)
  simpleActor ! SpacialMessage("some special content")


  // 2 - actors have information about their context and about themselves
  // context .slef === this in OOP

  case class SendMessageToYourSelf(contents: String)
  simpleActor ! SendMessageToYourSelf("I am an actor and I am proud of it")


  // 3 - actor can REPLY to messages
  val alice = system.actorOf(Props[SimpleActor], "alice")
  val bob = system.actorOf(Props[SimpleActor], "bob")

  case class SayHiTo(ref: ActorRef)
  alice ! SayHiTo(bob)

  // 4 - dead letrters
  alice ! "Hi" // reply to me

  // 5 - forwarding message
  // D -> A -> B
  // forwarding = sending the message with ORIGINAL sender
  case class WirelessPhoneMessage(content:String, ref : ActorRef)
  alice ! WirelessPhoneMessage("Hi", bob) // no Sender


  /**
    * Ex
    * 1. a Couner actor
    *    - Incremnt
    *    - Decremnt
    *    - Print
    *
    *  2. a Bank account as an actor
    *     receives
    *     - Deposit
    *     - withdraw an amount
    *     - Statement
    *     replies with
    *     - Success
    *     - Failure
    *
    *     interact with some other kind of actor
   */


  // DOMAIN of counter
  object Counter {
    case object Increment
    case object Decrement
    case object Print
  }

  class Counter extends Actor {
    import Counter._
    var count = 0

    override def receive: Receive = {
      case Increment => count +=1
      case Decrement => count -=1
      case Print => println(s"[counter] My current count is $count")
    }
  }
  import Counter._
  val counter = system.actorOf(Props[Counter],"myCounter")
  (1 to 5 ).foreach(_ => counter ! Increment)
  (1 to 3 ).foreach(_ => counter ! Decrement)

  counter ! Print


  //bank account
  


}
