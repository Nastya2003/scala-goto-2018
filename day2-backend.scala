import scala.util._
import scala.math.pow
import scala.math.sqrt
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import org.json4s.jackson.JsonMethods.{compact, render}
import Json.nameSurname
import scala.io.StdIn

object Main extends App {

  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher

  val route: Route =
    pathPrefix("hello") {
      pathPrefix("task1") {
        task1()
      } ~ pathPrefix("task2") {
        task2()
      } ~ pathPrefix("task3") {
        task3()
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

  //def nthRecursive(n: Int): Int = n * 2

  object task1 {
    def apply(): Route = {
      pathPrefix(Remaining) { name: String =>
        complete(nameSurname("Nastya", "Reznichenko", isPalindrom(name).toString))
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

  object day2Hard {
    def duplicate(n: Int, a: List[Char]): List[Char] = {
      a.map(i => List.fill(n)(i)).flatten
    }
  }
