import akka.actor._
import akka.pattern.{ask, pipe}
import akka.util.Timeout

import scala.collection._
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

//case class c1(ds: Int, a: Int, b: Int, c: Int, d: Int)
//case class c2(ds: Int, a: Int, b: Int, c: Int, d: Int)
//case class c3(ds: Int, a: Int, b: Int, c: Int, d: Int)
//case class act(cl: Any, ac: String, vl: Char, pr: Int, n: Int)
//
//
//class buy extends Actor with ActorLogging {
//
//}
//
//class sell extends Actor with ActorLogging {
//
//}

object Main extends App {
//  val system = ActorSystem("system")
//  val buy = system.actorOf(Props(new buy), "buy")
//  val sell = system.actorOf(Props(new sell), "sell")
  var balance: ListBuffer[(String, Int, Int, Int, Int, Int)] =
    ListBuffer(("c1", 1000, 10, 5, 15, 0), ("c2", 200,  5, 10, 0, 15), ("c3", 500, 15, 5, 10, 0))
  val actions: ListBuffer[(String, String, String, Int, Int)] =
    ListBuffer(("c1", "buy", "A", 7, 12), ("c2", "sell", "D", 10, 10), ("c3", "buy", "D", 10, 10), ("c3", "sell", "A", 7, 12))
}

object acts {
  import Main._
  def action(a: ListBuffer[(String, String, String, Int, Int)]): Unit = a match {
    case elem if (elem.head._2 == "buy" || elem.head._2 == "sell") => {
      search(elem.head, elem.tail)
      action(elem.tail)
     }
  }


  def search(e: (String, String, String, Int, Int), t: ListBuffer[(String, String, String, Int, Int)]): Unit = t match {
    case elem => {
      if (((elem.head._1 != e._1) && (elem.head._2 != e._2) && (elem.head._3 == e._3)) &&
        ((elem.head._4 == e._4) && (elem.head._5 != e._5))) {
        make(e, elem.head)
        search(e, elem.tail)
      } else _
    }
  }


  def make(a: (String, String, String, Int, Int), b: (String, String, String, Int, Int)): Unit = {
    if (a._2 == "sell") {
      findCheckSell(a, balance)
      findCheckBuy(b, balance)
    }
    else if (b._2 == "sell") {
      findCheckSell(a, balance)
      findCheckBuy(b, balance)
    }
    else _
  }


  def findCheckSell(sell: (String, String, String, Int, Int), bal: ListBuffer[(String, Int, Int, Int, Int, Int)]): Unit = {
    if ((sell._3 == "A") && (balance(findBal(sell._1, bal))._3 > sell._5)) {
      val x = balance(findBal(sell._1, bal))._3
      val old = balance(findBal(sell._1, bal))
      val new2 = (old._1, old._2, old._3 - sell._5, old._4, old._5, old._6)
      balance(findBal(sell._1, bal)) = new2

      val old2 = actions(findAct(sell, actions))
      val new3 = (old2._1, old2._2, old2._3, old2._4, old2._5 + x)
      actions(findAct(sell, actions))   = new3
    }

    else if ((sell._3 == "B") && (balance(findBal(sell._1, bal))._4 > sell._5)) {
      val x = balance(findBal(sell._1, bal))._4
      val old = balance(findBal(sell._1, bal))
      val new2 = (old._1, old._2, old._3, old._4 - sell._5, old._5, old._6)
      balance(findBal(sell._1, bal)) = new2

      val old2 = actions(findAct(sell, actions))
      val new3 = (old2._1, old2._2, old2._3, old2._4, old2._5 + x)
      actions(findAct(sell, actions))   = new3
    }

    else if ((sell._3 == "C") && (balance(findBal(sell._1, bal))._5 > sell._5)) {
      val x = balance(findBal(sell._1, bal))._5
      val old = balance(findBal(sell._1, bal))
      val new2 = (old._1, old._2, old._3, old._4, old._5 - sell._5, old._6)
      balance(findBal(sell._1, bal)) = new2

      val old2 = actions(findAct(sell, actions))
      val new3 = (old2._1, old2._2, old2._3, old2._4, old2._5 + x)
      actions(findAct(sell, actions))   = new3
    }

    else if ((sell._3 == "D") && (balance(findBal(sell._1, bal))._6 > sell._5)) {
      val x = balance(findBal(sell._1, bal))._6
      val old = balance(findBal(sell._1, bal))
      val new2 = (old._1, old._2, old._3, old._4, old._5, old._6 - sell._5)
      balance(findBal(sell._1, bal)) = new2

      val old2 = actions(findAct(sell, actions))
      val new3 = (old2._1, old2._2, old2._3, old2._4, old2._5 + x)
      actions(findAct(sell, actions))   = new3
    }
    else _
   }


  def findCheckBuy(buy: (String, String, String, Int, Int), bal: ListBuffer[(String, Int, Int, Int, Int, Int)]): Unit = {
    if (balance(findBal(buy._1, balance))._2 > actions(findAct(buy, actions))._4 * actions(findAct(buy, actions))._5) {

    }
    
  }


  def findBal(a: String, bal: ListBuffer[(String, Int, Int, Int, Int, Int)], k: Int = 0): Int = bal match {
    case elem => {
      if (a == elem.head._1) k
      else findBal(a, elem.tail, k + 1)
    }
  }

  def findAct(a: (String, String, String, Int, Int), act: ListBuffer[(String, String, String, Int, Int)], k: Int = 0): Int = act match {
    case elem => {
        if (a == elem) k
        else findAct(a, elem.tail, k + 1)
    }
  }
}

