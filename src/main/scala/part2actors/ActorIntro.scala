package part2actors

import akka.actor.{Actor, ActorSystem, Props}

object ActorIntro extends App {

  /// part1 - actor system
  val actorSystem = ActorSystem("FirstActorSystem")
  println(actorSystem.name)


  /// part2 - create
  // word count actor

  class WordCounterActor extends Actor {
    // internnal data
    var totalWord = 0;

    // behavior
    override def receive: PartialFunction[Any,Unit] = {
      case message: String =>
        println(s"[word counter] I have receive $message")
        totalWord += message.split(" ").length
      case msg => println(s"[word counter] I cannot understand ${msg.toString}")
    }
  }


  /// part3 - instantiate our actor
  val wordCounter = actorSystem.actorOf(Props[WordCounterActor], "WordCounter")
  val anotherWordCounter = actorSystem.actorOf(Props[WordCounterActor], "anotherWordCounter")

  /// part 4 - communicate!
  wordCounter ! "learning akka"
  anotherWordCounter ! "A different message"


  object Person {
    def props(name:String) = Props(new Person(name))
  }

  class Person(name:String) extends Actor {
    override def receive: Receive = {
      case "hi" => println(s"hi my name is $name")
      case _ =>
    }
  }

  //val person = actorSystem.actorOf(Props(new Person("Saleh")))
  val person = actorSystem.actorOf(Person.props("Saleh"))
  person ! "hi"

}
