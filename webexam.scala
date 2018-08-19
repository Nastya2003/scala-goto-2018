// задание 3
import akka.actor.{Actor, ActorLogging, Props}
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import scala.io.StdIn
import scala.xml.Utility.trim
import akka.actor._
import scala.collection._

case class set(a: String, b: Int)
case class bet(a: String, b: Int)
case object Start

class aucActor extends Actor with ActorLogging {
  val state: mutable.Map[String, Int] = mutable.Map.empty
  var currPrice = 0
  def receive = {
    case r: set => state += r.a -> r.b
    case r: bet =>
    {
      if (state.isEmpty || r.a == "stop")
        context.system.terminate()
      else if (r.b == -1) state.remove(r.a)
      else if (state.get(r.a) == None)
        log.warning(s"${r.a} was already bought")
      else if (state(r.a) <= r.b) {
        log.warning(s"${r.a} bought by ${r.b}")
        state -= r.a
      }
      else if (currPrice < r.b) {
        currPrice = r.b
        log.warning(s"${r.a} current prise is: ${currPrice}")
      }
      else log.warning(s"${r.a} minimum price is: ${r.b}")
    }
  }
}


object Main extends App {
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  val aucActor = system.actorOf(Props(new aucActor), "aucActor")

  val route: Route =
    path("main") {
      val res = trim {
        <message>    This Is The Main Page    </message>
      }.toString.filter(_ != '\n')
      complete(res) } ~ path("main" / "set" / """[^_]+""".r ~ "_" ~ IntNumber) { (subject: String, price: Int) =>
        aucActor ! set(subject, price)
        complete(subject + " first price " + price)
        } ~ path("main" / "bet" / """[^_]+""".r ~ "_" ~ IntNumber) {  (subject: String, price: Int) =>
          aucActor ! bet(subject, price)
          complete("Done")
        } ~ path("main" / "end") {
          aucActor ! bet("stop", 0)
          complete("End of auction")
        } ~ path("main" / "remove" / """[^_]+""".r) { (subject: String) =>
          aucActor ! bet(subject, -1)
          complete("Subject remain from Map")
        } ~ path("main" / "increase" / """[^_]+""".r ~ "_" ~ IntNumber) { (subject: String, price: Int) =>
          aucActor ! set(subject, price)
          complete("New first prise is " + price)
      }


  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())
}


// задание 1
object task1 {
  def task1(a: List[Future[Try[Option[Int]]]], b: List[Future[Try[Option[Int]]]] = List()): List[Future[Try[Option[Int]]]] = a match {
    case (Nil) => Nil
   case (elem :: tail) => task1(tail, tail :+ elem.map(_.map(_.map(elem => elem * 10))))
  }
}
