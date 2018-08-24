import Main.cat
import akka.http.scaladsl.server.PathMatchers.Remaining
import akka.http.scaladsl
import scala.collection.mutable.ListBuffer
import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Route
import scala.io.StdIn

case class Cat(name: String, isMale: Boolean, breed: String, age: Int)

object Main extends App {
  implicit val system1 = ActorSystem("my-system")
  implicit val executionContext = system1.dispatcher
  implicit val materializer = ActorMaterializer()



  val cat: ListBuffer[Cat] = ListBuffer()
  val route: Route =
    pathPrefix("add") {
      add()
    } ~ pathPrefix("take") {
      take()
    } ~ pathPrefix("mate") {
      mate()
    }

    val bindingFuture = scaladsl.Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system1.terminate())
  }


object add {
  import Main._
  def apply(): Route = {
    path(Remaining) { name: String =>
      add(name)
      complete("Котик успешно добавлен")
    }
  }

  def add(a: String): Main.cat.type = {
    val cat1: Cat = Cat(a.split("_")(0), a.split("_")(1).toBoolean, a.split("_")(2), a.split("_")(3).toInt)
    cat += cat1
  }
}

object take {
  import mate._
  def apply(): Route = {
    path(Remaining) { name: String =>
     take(name)
      complete("Поздравляем! Вы забрали котика домой!")
    }
  }

  def take(a: String): Boolean = {
    val k = find(a, cat)
    if (k != 0) {
      cat -= cat(k)
      true
    }
    else false
  }
}

object mate {
  def apply(): Route = {
    path(Remaining) { name: String =>
      mate(name)
      complete("Котики успешно спаренны. Ждем котенка!")
    }
  }

  def mate(a: String): Boolean = {
    val cat1 = a.split("_")(0)
    val cat2 = a.split("_")(1)

    if (cat.contains(cat1) && cat.contains(cat2)) {
      val r1 = scala.util.Random
      r1.nextInt()
      var r11: Boolean = false
      if (r1 == 0) r11 = false
      else r11 = true

      val r2 = scala.util.Random
      r2.nextBoolean()
      val k1 = find(cat1, cat)
      val k2 = find(cat2, cat)
      var br: String = ""
      if (r2 == true) {
        val cat11 = cat(k1)
        br = cat11.breed
      }
      else {
        val cat22 = cat(k2)
        br = cat22.breed
      }
      val newcat: Cat = Cat(cat1+cat2, r11, br, 0)
      cat += newcat
      true
    }
    else false
  }

  def find(a: String, b: ListBuffer[Cat], k: Int = 0): Int = {
    if (b nonEmpty) {
      if (b.head.name == a) k
      else find(a, b.tail, k + 1)
    }
    else k
  }
}
