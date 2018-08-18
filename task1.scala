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
  }
