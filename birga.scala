import akka.actor._
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import scala.collection._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

case class c1(ds: Int, a: Int, b: Int, c: Int, d: Int)
case class c2(ds: Int, a: Int, b: Int, c: Int, d: Int)
case class c3(ds: Int, a: Int, b: Int, c: Int, d: Int)
case class act(cl: Any, ac: String, vl: Char, pr: Int, n: Int)


class buy extends Actor with ActorLogging {

}

class sell extends Actor with ActorLogging {

}

object Main extends App {
  val system = ActorSystem("system")
  val buy = system.actorOf(Props(new buy), "buy")
  val sell = system.actorOf(Props(new sell), "sell")
  val balance = List("c1 1000 10 5 15 0", "c2 200  5 10 0 15", "c3 500  15 5 10 0")
  val actions = List("c1 buy  A 7  12", "c2 sell D 10 10", "c3 buy  D 10 10", "c3 sell A 7  12")

}

object acts {
  def action(a: List[String]): Unit = a match {
    case Nil => Nil
    case (elem :: tail) => {
      val elem1 = elem.split(" ")
      elem1 match {
        case (elem2 :: tail1) => {
          if (elem2 == "buy") search(elem, tail)
          else if (elem2 == "sell") search(elem, tail)
          else action(tail)
        }
      }
    }
  }

  def search(e: String, a: List[String]): Unit = a match {
    case Nil => Nil
    case (elem :: tail) =>
      if ((e(1) != elem(1)) && (e(2) != elem(2)) && (e(3) == elem(3)) && (e(4) == elem(4)) (e(5) == elem(5))) {
        make(e, elem)
      } else _
  }

  def chechSell(a: act, b: List[String]): Boolean {

  }

  def make(a: String, b: String): Unit = {
    if (a(2) == "sell") {

    }
  }
}
