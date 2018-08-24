object Main extends App {_
 import acts._

 var balance: ListBuffer[(String, Int, Int, Int, Int, Int)] =
   ListBuffer(("c1", 1000, 10, 5, 15, 0), ("c2", 200,  5, 10, 0, 15), ("c3", 500, 15, 5, 10, 0))
 val actions: ListBuffer[(String, String, String, Int, Int)] = ListBuffer()
   //ListBuffer(("c1", "buy", "A", 7, 12), ("c2", "sell", "D", 10, 10), ("c3", "buy", "D", 10, 10), ("c3", "sell", "A", 7, 12))
 action(actions)
 println(balance)
 println(actions)

 val route: Route =
//    pathPrefix("exchange") {
//      val res = trim {
//        <message>Добро пожаловать на биржу! /n Введите "add", чтобы добавить запрос</message>
//      }.toString.filter(_ != '\n')
//      complete(res) ~
       path("exchange" / "add" / Remaining) {
         add()
       }
   }


 val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

 println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
 StdIn.readLine()
 bindingFuture
   .flatMap(_.unbind())
   .onComplete(_ => system.terminate())
}

object add {
 import Main._
 import acts._

 def apply(): Route = {
   path(Remaining) { name: String =>
     complete("Done")
   }
 }

 def go(name: String): Unit = {
   val add1 = Remaining.toString ().split (" ").toList
   val tup: (String, String, String, Int, Int) = (add1 (0), add1 (1), add1 (2), add1 (3).toInt, add1 (4).toInt)
   actions += tup
   action(actions)
 }
}


object acts {
 import Main._

 def action(a: ListBuffer[(String, String, String, Int, Int)]): Boolean = {
   if (a nonEmpty) {
     if (a.head._2 == "buy" || a.head._2 == "sell") {
       search(a.head, a.tail)
       action(a.tail)
       true
     }
     else false
   }
   else false
 }


 def search(e: (String, String, String, Int, Int), t: ListBuffer[(String, String, String, Int, Int)]): Boolean = {
   if (t nonEmpty) {
     if (((t.head._1 != e._1) && (t.head._2 != e._2) && (t.head._3 == e._3)) &&
       ((t.head._4 == e._4) && (t.head._5 == e._5))) {
       make(e, t.head)
       search(e, t.tail)
       true
     }
     else false
   }
   else false
 }


 def make(a: (String, String, String, Int, Int), b: (String, String, String, Int, Int)): Boolean = {
   if (a._2 == "sell") {
     if (checkSell(a, b, balance) && checkBuy(a, b, balance)) {
       doSell(a, balance)
       doBuy(b, balance)
       delete(a, b, actions)
       true
     }
     else false
   }
   else if (b._2 == "sell") {
     if (checkSell(b, a, balance) && checkBuy(b, a, balance)) {
       doSell(b, balance)
       doBuy(a, balance)
       delete(a, b, actions)
       true
     }
     else false
   }
   else false
 }


 def checkSell(sell: (String, String, String, Int, Int), buy: (String, String, String, Int, Int),
               bal: ListBuffer[(String, Int, Int, Int, Int, Int)]): Boolean = {
   if ((sell._3 == "A") && (balance(findBal(sell._1, bal))._3 > sell._5)) true
   else if ((sell._3 == "B") && (balance(findBal(sell._1, bal))._4 > sell._5)) true
   else if ((sell._3 == "C") && (balance(findBal(sell._1, bal))._5 > sell._5)) true
   else if ((sell._3 == "D") && (balance(findBal(sell._1, bal))._6 > sell._5)) true
   else false
 }


 def checkBuy(sell: (String, String, String, Int, Int), buy: (String, String, String, Int, Int),
              bal: ListBuffer[(String, Int, Int, Int, Int, Int)]): Boolean = {
   if (balance(findBal(buy._1, balance))._2 > actions(findAct(buy, actions))._4 * actions(findAct(buy, actions))._5) true
   else false
 }


 def doSell(sell: (String, String, String, Int, Int), bal: ListBuffer[(String, Int, Int, Int, Int, Int)]): Boolean = {
   if ((sell._3 == "A") && (balance(findBal(sell._1, bal))._3 > sell._5)) {
     val old = balance(findBal(sell._1, bal))
     val new2 = (old._1, old._2 + sell._4 * sell._5, old._3 - sell._5, old._4, old._5, old._6)
     balance(findBal(sell._1, bal)) = new2
     true
   }

   else if ((sell._3 == "B") && (balance(findBal(sell._1, bal))._4 > sell._5)){
     val old = balance(findBal(sell._1, bal))
     val new2 = (old._1, old._2 + sell._4 * sell._5, old._3, old._4 - sell._5, old._5, old._6)
     balance(findBal(sell._1, bal)) = new2
     true
   }

   else if ((sell._3 == "C") && (balance(findBal(sell._1, bal))._5 > sell._5)) {
     val old = balance(findBal(sell._1, bal))
     val new2 = (old._1, old._2 + sell._4 * sell._5, old._3, old._4, old._5 - sell._5, old._6)
     balance(findBal(sell._1, bal)) = new2
     true
   }

   else if ((sell._3 == "D") && (balance(findBal(sell._1, bal))._6 > sell._5)) {
     val old = balance(findBal(sell._1, bal))
     val new2 = (old._1, old._2 + sell._4 * sell._5, old._3, old._4, old._5, old._6 - sell._5)
     balance(findBal(sell._1, bal)) = new2
     true
   }
   else false
  }


 def doBuy(buy: (String, String, String, Int, Int), bal: ListBuffer[(String, Int, Int, Int, Int, Int)]): Boolean = {
   val money = actions(findAct(buy, actions))._4 * actions(findAct(buy, actions))._5
   if (buy._3 == "A" && balance(findBal(buy._1, balance))._2 > money) {
     val old = balance(findBal(buy._1, balance))
     val new2 = (old._1, old._2 - money, old._3 + buy._5, old._4, old._5, old._6)
     balance(findBal(buy._1, balance)) = new2
     true
   }

   else if (buy._3 == "B" && balance(findBal(buy._1, balance))._2 > money) {
     val old = balance(findBal(buy._1, balance))
     val new2 = (old._1, old._2 - money, old._3, old._4 + buy._5, old._5, old._6)
     balance(findBal(buy._1, balance)) = new2
     true
   }

   else if (buy._3 == "C" && balance(findBal(buy._1, balance))._2 > money) {
     val old = balance(findBal(buy._1, balance))
     val new2 = (old._1, old._2 - money, old._3, old._4, old._5 + buy._5, old._6)
     balance(findBal(buy._1, balance)) = new2
     true
   }

   else if (buy._3 == "D" && balance(findBal(buy._1, balance))._2 > money) {
     val old = balance(findBal(buy._1, balance))
     val new2 = (old._1, old._2 - money, old._3, old._4 , old._5, old._6 + buy._5)
     balance(findBal(buy._1, balance)) = new2
     true
   }

   else false
 }


 def findBal(a: String, bal: ListBuffer[(String, Int, Int, Int, Int, Int)], k: Int = 0): Int = bal match {
   case elem => {
     if (a == elem.head._1) k
     else findBal(a, elem.tail, k + 1)
   }
 }


 def findAct(a: (String, String, String, Int, Int), act: ListBuffer[(String, String, String, Int, Int)], k: Int = 0): Int = {
   if (act nonEmpty) {
       if (a == act.head) k
       else findAct(a, act.tail, k + 1)
   }
   else k
 }

 def delete(sell: (String, String, String, Int, Int), buy: (String, String, String, Int, Int),
            act: ListBuffer[(String, String, String, Int, Int)]): Unit = {
   actions.remove(findAct(sell, act))
   actions.remove(findAct(buy, act))
 }
}
