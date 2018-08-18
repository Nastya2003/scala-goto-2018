import akka.actor._
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import scala.collection._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class set(a: String, b: Int)
case class bet(a: String, b: Int)
case object Start

class aucActor extends Actor with ActorLogging {
  val state: mutable.Map[String, Int] = mutable.Map.empty
  var currPrice = 0
  def receive = {
    case r: set => state += r.a -> r.b
    case r : bet if (state.get(r.a) == None) => log.warning(s"${r.a} was already bought")
    case r: bet if state(r.a) <= r.b => {
      log.warning(s"${r.a} bought by ${r.b}")
      state -= r.a
    }
    case r: bet if currPrice < r.b => {
      currPrice = r.b
      log.warning(s"${r.a} current prise is: ${currPrice}")
    }
    case r: bet => log.warning(s"${r.a} minimum price is: ${r.b}")
  }
  if (state.isEmpty) context.system.terminate()
}

class mainActor extends Actor with ActorLogging {
  val state: mutable.Map[String, Int] = mutable.Map.empty
  val aucActor = context.actorOf(Props(new aucActor), "aucActor")

  implicit val timeout = Timeout(5 seconds)

  override def preStart() {
    self ! Start
  }

  def receive = {
    case Start =>
      aucActor ! set("gold", 150)
      log.warning(s"Gold first prise: 150")
      aucActor ! set("silver", 100)
      log.warning(s"Silver first prise: 100")
      aucActor ! set("brilliant", 400)
      log.warning(s"Brilliant first prise: 400")

      aucActor ! bet("gold", 120)
      aucActor ! bet("gold", 80)
      aucActor ! bet("silver", 90)
      aucActor ! bet("gold", 200)
      aucActor ! bet("brilliant", 300)
      aucActor ! bet("silver", 70)
      aucActor ! bet("brilliant", 250)
      aucActor ! bet("silver", 120)
      aucActor ! bet("brilliant", 400)
  }
}

object Main extends App {
  val system = ActorSystem("system")
  val mainActor = system.actorOf(Props(new mainActor), "mainActor")
}





