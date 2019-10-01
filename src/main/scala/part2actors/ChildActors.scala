package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChildActors extends App {
  // Actor can create other actors

  object Parent {
     case class CreateChild(name:String)
     case class TellChild(message:String)
  }

  class Parent extends Actor {
    import Parent._

    override def receive: Receive = {
      case CreateChild(name) =>
        println(s"${self.path} creating child")
        //create a new actor right HERE
        val childRef = context.actorOf(Props[Child],name)
        context.become(withChild(childRef))
    }

    def withChild(child: ActorRef) : Receive = {
      case TellChild(message) =>
        if(child != null) child forward message
    }
  }

  class Child extends Actor {
    override def receive: Receive = {
      case message => println(s"${self.path} I got : $message")
    }
  }

  import Parent._

  val system = ActorSystem("childActor")
  val parent = system.actorOf(Props[Parent],"parent")

  parent ! CreateChild("child")
  parent ! TellChild("hay Kid!")


  // actor hierarchies
  // parent -> child -> grandChild
  //        -> child2 ->


  /**
    * Guardian actors (top-level)
    * - /system = system guardian
    * - /user = user-level guardian
    * - / = the root guardian
    */


  /**
    * Actor selection
    *
    */

  val childSelection = system.actorSelection("/user/parent/child")
  childSelection ! "I found you!"

  /**
    * Danger!
    *
    * Never PASS MUtABLE ACTOR STATE , OR 'THIS' TO CHILD ACTOR
    * NEVER IN LIFE
    *
    * */


  object NaiveBankAcount {
    case class Deposit(amount: Int)
    case class Withdraw(amount: Int)
    case object InitializeAccount
  }

  class NaiveBankAcount extends Actor {

    import CreditCard._
    import NaiveBankAcount._

    var amount = 0

    override def receive: Receive = {
      case InitializeAccount =>
        val creditCardRef = context.actorOf(Props[CreditCard], "card")
        creditCardRef ! AttachedAccount(this)
      case Deposit(funds) => deposit(funds)
      case Withdraw(funds) => withdraw(funds)
    }

    def deposit(funds: Int) = {
      println(s"${self.path} depositing $funds on top of $amount")
      amount += funds
    }

    def withdraw(funds: Int) = {
      println(s"${self.path} withdraw $funds on top of $amount")
      amount -= funds
    }
  }

  object CreditCard {
    case class AttachedAccount(banckAccount: NaiveBankAcount) // !!
    case object CheckStatus
  }

  class CreditCard extends Actor {
    import CreditCard._

    override def receive: Receive = {
      case AttachedAccount(account) => context.become(attachedTo(account))
    }

    def attachedTo(account: NaiveBankAcount) : Receive = {
      case CheckStatus => println(s"${self.path} your message has been processed.")
        account.withdraw(1)
    }
  }

  import NaiveBankAcount._
  import CreditCard._

  val bankAccountRef = system.actorOf(Props[NaiveBankAcount],"account")
  bankAccountRef ! InitializeAccount
  bankAccountRef ! Deposit(100)

  Thread.sleep(500)
  val ccSelection = system.actorSelection("/user/account/card")
  ccSelection ! CheckStatus


  // WRONG !!!!!!


}
