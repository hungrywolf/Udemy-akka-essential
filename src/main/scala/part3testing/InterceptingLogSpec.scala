package part3testing

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.testkit.{EventFilter, ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}


class InterceptingLogSpec extends
  TestKit(ActorSystem("TimedAssertionSpec", ConfigFactory.load().getConfig("interceptingLogMessages")))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import InterceptingLogSpec._

  val item = "Rock the JVM Akka course"
  val creditCard = "1234-1234-1234-124"
  val invalidCreditCard = "0000-0000-0000-0000"

  "A checkout flow" should {
    "correctly log  the dispatch of an order" in {
      EventFilter.info(pattern = s"Order [0-9]+ for $item has bess dispatched.", occurrences = 1) intercept {
        // our test code
        val checkoutRef = system.actorOf(Props[CheckoutActor])
        checkoutRef ! CheckOut(item, creditCard)
      }
    }

    "freak out if the payment is denied" in {
      EventFilter[RuntimeException](occurrences = 1) intercept {
        val checkoutRef = system.actorOf(Props[CheckoutActor])
        checkoutRef ! CheckOut(item, invalidCreditCard)
      }
    }
  }

}

object InterceptingLogSpec {

  case class CheckOut(item:String, creditCard: String)
  case class AuthorizeCard(creditCard: String)
  case object PaymentAccepted
  case object PaymentDenied
  case class DispatchOrder(item: String)
  case object OrderConfirmed

  class CheckoutActor extends Actor {
    private val paymentManager = context.actorOf(Props[PaymentManager])
    private val fulfillmentMaanger = context.actorOf(Props[FulfillmentManager])

    override def receive: Receive = awaitingCheckout

    def awaitingCheckout: Receive = {
      case CheckOut(item, credit) =>
        paymentManager ! AuthorizeCard(credit)
        context.become(pendingPayment(item))
    }

    def pendingPayment(item: String): Receive = {
      case PaymentAccepted =>
        fulfillmentMaanger ! DispatchOrder(item)
        context.become(pendingFulfilment(item))
      case PaymentDenied => throw new RuntimeException("I can't handle this anymore")
    }

    def pendingFulfilment(item: String): Receive = {
      case OrderConfirmed =>
        context.become(awaitingCheckout)
    }
  }

  class PaymentManager extends Actor {
    override def receive: Receive = {
      case AuthorizeCard(card) =>
        if (card.startsWith("0")) sender() ! PaymentDenied
        else {
          Thread.sleep(4000)
          sender() ! PaymentAccepted
        }
    }
  }

  class FulfillmentManager extends  Actor with ActorLogging {
    var orderId = 43

    override def receive: Receive = {
      case DispatchOrder(item: String) =>
        orderId += 1
        log.info(s"Order $orderId for $item has bess dispatched.")
        sender() ! OrderConfirmed
    }
  }
}
