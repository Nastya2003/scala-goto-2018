 object day2Hard {
    def duplicate(n: Int, a: List[Char]): List[Char] = {
      a.map(i => List.fill(n)(i)).flatten
    }
  }

  object day3Base {
    def isOdd(x: Int): Boolean = x % 2 == 0

    def isEven(x: Int): Boolean = x % 2 != 0

    def sumOfDigits(x: Int): Int =
      x / 10 + sumOfDigits(x - x / 10)

    def compositionOfDigits(x: Int): Int =
      (x / 10) * sumOfDigits(x - x / 10)

    def nthGreatestDivisor(x: Int, b: List[Int] = List(), a: List[Int]): List[Int] = a match {
      case (elem :: Nil) if (x % elem == 0) => b :+ elem
      case (elem :: tail) if (x % elem == 0) => nthGreatestDivisor(tail.toString.toInt, b :+ elem, a)
    }

    def nthGreatestDivisor2(b: List[Int], n: Int): Int = b match {
      case (Nil) => 0
      case (elem :: tail) => nthGreatestDivisor2(tail, n-1)
    }

    def numOfDivisors(b: List[Int]): Int = b.length

    def sumOfDivisors(b: List[Int]): Int = b match {
      case (Nil) => 0
      case (elem :: tail) => elem + sumOfDivisors(tail)
    }

    def isPrime(a: Int): Boolean = {
      var b: List[Int] = List()
      var c: List[Int] = List()
      b = nthGreatestDivisor(a, b, c)
      if (b.length == 2) true else false
    }
  }
