def task1(a: List[Future[Try[Option[Int]]]], b: List[Future[Try[Option[Int]]]] = List()): List[Future[Try[Option[Int]]]] = a match {
  case (Nil) => Nil
  case (elem :: tail) => task1(tail, tail :+ elem.future.map(_.map(_.map(elem => elem * 10))))
}
