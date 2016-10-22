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
      (c, r) match {
        case (column, row) if column < 0 || row < 0 => 0
        case (column, row) if column == row || row == 0 || column == 0 => 1
        case (column, row) => pascal(column - 1, row - 1) + pascal(column, row - 1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def isBalanced(chars: List[Char], balance: Int): Int = {
        if (balance < 0) -1
        else
          chars match {
            case Nil => balance
            case head::tail => head match {
              case '(' => isBalanced(tail, balance + 1)
              case ')' => isBalanced(tail, balance - 1)
              case _ => isBalanced(tail, balance)
            }
          }
      }

      isBalanced(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count(coins: List[Int], denominationValuesCount: Int, amount: Int): Int = {
        if (amount == 0) 1
        else if (amount < 0) 0
        else if (denominationValuesCount <= 0 && amount >= 1) 0
        else
          count(coins, denominationValuesCount - 1, amount) +
            count(coins, denominationValuesCount, amount - coins.apply(denominationValuesCount - 1))
      }
      count(coins, coins.size, money)
    }
  }
