 object day2Hard {
    def duplicate(n: Int, a: List[Char]): List[Char] = {
      a.map(i => List.fill(n)(i)).flatten
    }
  }

  object day3 {
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

    def rle1(a: List[List[Char]], b: List[Any] = List()): List[Any] = a match {
      case (Nil) => Nil
      case (i :: tail) => if (i.length == 1) rle1(tail, b :+ i(0)) else rle1(tail, b :+ i)
    }

    def rle21(a: List[Char], b: List[Char] = List()): List[Char] = a match {
      case (Nil) => Nil
      case (i :: tail) => rle21(tail, b :+ i)
    }

    def rle2(a: List[List[Char]], b: List[Char] = List()): List[Char] = a match {
      case (Nil) => Nil
      case (i :: tail) => if (i.length == 1) rle2(tail, b :+ i(0)) else i match {
         case (Nil) => Nil
         case (a :: tail) => rle21(tail, b :+ a)
      }
    }

    def plusMinus(a: List[Int], pred: Int = 0): Int = a match {
      case (Nil) => 0
      case (i :: tail) => if (i >= 0 && pred >= 0) 0 + plusMinus(tail, i) else 1 + plusMinus(tail, i)
    }

    def Letters(a: List[Char], vowel: List[Char] = List('a', 'e', 'u', 'i', 'o', 'y')): Int = a match {
    case (Nil) => 0
    case (i :: tail) => if (vowel.contains(i)) 1 + Letters(tail, vowel) else 0 + Letters(tail, vowel)
    }

    def isBinary(a: Int): Boolean = {
      if (a == 0 || a == 1) false
      else if (a % 2 == 0) true
      else isBinary(a / 2)
    }
   
    def binaryDivisors(a: Int, b: Int = 1, c: List[Int] = List()): List[Any] = {
     if (isBinary(a)) {
       if (a >= b * 2) binaryDivisors(a, b, c :+ b)
     }
     else Nil
    }
}
