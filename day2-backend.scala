import akka.actor.{Actor, ActorLogging, Props}
import akka.util.Timeout
import scala.concurrent.Future
import scala.util.Try
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.Done
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.{HttpRequest, StatusCodes}
import akka.http.scaladsl.unmarshalling.Unmarshal
import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS
import scala.io.StdIn
import scala.concurrent.{Await, Future}
//import scala.scalajs.niocharset.StandardCharsets
import org.json4s._
import org.json4s.jackson.JsonMethods._
import Json.nameSurname
import org.json4s.jackson.JsonMethods._
import scala.xml._
import scala.xml.Utility.trim

object Main extends App {
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val route: Route =
    pathPrefix("hello") {
      pathPrefix("task1") {
        task1()
      } ~ pathPrefix("task2") {
        task2()
      } ~ pathPrefix("task3") {
        task3()
      } ~ pathPrefix("task4") {
        task4()
      }
      //      ~ pathPrefix(Remaining) {
      //        helloRoute()
      //      }
    }

  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  StdIn.readLine() // let it run until user presses return
  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ => system.terminate()) // and shutdown when done
}


object helloRoute {
  def apply(): Route = {

    pathPrefix(Remaining) { name: String =>

      complete(nameSurname("Nastya", "Reznichenko", "There is no such task"))
    }
  }
}


object task1 {
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  def apply(): Route = {
    pathPrefix(Remaining) { name: String =>
      extractRequest { req =>
        println(req.entity)
        println(req.protocol)
        println(req.method)
        println(req.headers)
        val responseAsString: Future[String] = Unmarshal(req.entity).to[String]
        val incomeString: String = Await.result(responseAsString, Duration(500, MILLISECONDS))

        println(incomeString)
        val a: JValue = parse(incomeString)
        val fin: List[(String, BigInt, String)] = for {
          JObject(child) <- a
          JField("name", JString(name)) <- child
          JField("id", JInt(id)) <- child
          JField("surname", JString(surname)) <- child
        } yield (name, id, surname)
        print(fin.head)

        val double = fin.head._2 * 2

        println(double)
        complete(nameSurname("Nastya", "Reznichenko", isPalindrom(name).toString))
      }
    }
  }

  def isPalindrom(n: String): Boolean = n == n.reverse
}


object task2 {
  def apply(): Route = {
    pathPrefix(Remaining) { name: String =>
      complete(nameSurname("Nastya", "Reznichenko", reverse(name).toString))
    }
  }

  def reverse(n: String): String = n.toList match {
    case (elem :: s) => reverse (n.tail) + elem
    case (_) => ""
  }
}



object task3 {
  def apply(): Route = {
    pathPrefix(Remaining) { name: String =>
      complete(nameSurname("Nastya", "Reznichenko", length(name).toString))
    }
  }

  def length(n: String): Int = {
    n.toList
    if (n.length < 1) 0 else 1 + length(n.tail)

  }
}

object task4 {
  def apply(): Route = {
    pathPrefix(Remaining) { name: String =>
      complete(nameSurname("Nastya", "Reznichenko", xml("GoToAugust", 2018)))
    }
  }
  def xml (a: String, b: Int): String = trim {
    <Head>
    <Name> ${a} </Name>
    <ID> ${b} </ID>
      </Head>
  }.toString.filter(_ != '\n')
}
