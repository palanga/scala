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
  // TODO make it tail recursive
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, _) => 1
    case (_, 0) => 1
    case (_, 1) => 1
    case (col, row) if row == col => 1
    case (col, row) => pascal(col - 1, row - 1) + pascal(col, row - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceAux(count: Int, chars: List[Char]): Boolean = (count, chars) match {
      case (c, _) if c < 0 => false
      case (c, List()) => c == 0
      case (c, x :: xs) if x == '(' => balanceAux(c + 1, xs)
      case (c, x :: xs) if x == ')' => balanceAux(c - 1, xs)
      case (c, _ :: xs) => balanceAux(c, xs)
    }

    balanceAux(0, chars)
  }


  /**
    * Exercise 3
    */
  // TODO tail recursive
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (_, List()) => 0
    case (m, _) if m < 0 => 0
    case (0, _) => 1
    case (m, c :: cs) => countChange(m - c, c :: cs) + countChange(m, cs)
  }
}
