import akka.actor._
import akka.pattern.{ask, pipe}
import akka.util.Timeout

import scala.collection._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class SetRequest(key: String, value: String)
case class GetRequest(key: String)
case class GetResponse(key: Option[String])
case class set(a: String, b: Int)
case class bet(a: String, b: Int)
case object Start

class aucActor extends Actor with ActorLogging {
  val state: mutable.Map[String, Int] = mutable.Map.empty

  def receive = {
    case r: set =>
      state += r.a -> r.b
    case r: bet =>
      sender ! bet(state.get(r.b))
    case r =>
      log.warning(s"Unexpected: $r")
  }
}

class mainActor extends Actor with ActorLogging {
  val state: mutable.Map[String, String] = mutable.Map.empty
  val mapActor = context.actorOf(Props(new mapActor), "mapActor")

  implicit val timeout = Timeout(5 seconds)

  override def preStart() {
    self ! Start
  }

  def receive = {
    case Start =>
      val fp = 150
      aucActor ! set("gold", fp)
      log.warning(s"First prise: $fp")

    case r: bet =>
      if(r.b <= fp) log.warning(s"Prise if the same: $r")
      else log.warning(s"New prise: $r.b")
      context.system.terminate()
  }
}


object Main extends App {
  val system = ActorSystem("system")
  val mainActor = system.actorOf(Props(new mainActor), "mainActor")
