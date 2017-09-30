package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1 else {
        pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balanceIter(xs: List[Char], parenthesesCount: Int): Boolean = {
        if (xs isEmpty) {
          return parenthesesCount == 0
        }
        else {
          if (parenthesesCount < 0)
            return false
          else
            balanceIter(xs.tail, handleParenthesesCount(xs.head, parenthesesCount))
        }
      }

      def handleParenthesesCount(currentChar: Char, count: Int): Int = {
        if (currentChar == '(')
          return count + 1
        else if (currentChar == ')')
          return count - 1
        else
          return count
      }

      balanceIter(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0)
        return 1
      else if (money > 0 && !coins.isEmpty)
        return countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else
        return 0
      }
}
