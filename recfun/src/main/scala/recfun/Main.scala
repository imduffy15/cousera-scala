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
    def pascalInter(c: Int, r: Int): Int = {
      if (c == 0 || r == c) 1
      else pascalInter(c - 1, r - 1) + pascalInter(c, r - 1)
    }
    pascalInter(c, r)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceInter(stack: List[Char], chars: List[Char]): Boolean = {
      chars match {
        case Nil => stack.isEmpty
        case '(' :: tail =>
          balanceInter('(' :: stack, chars.tail)
        case ')' :: tail =>
          if (stack.isEmpty) false
          else balanceInter(stack.tail, chars.tail)
        case x :: tail =>
          balanceInter(stack, chars.tail)
      }
    }
    balanceInter(List(), chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = money match {
    case 0 => 1
    case x if x < 0 => 0
    case _ => coins match {
      case list if list.isEmpty => 0
      case head :: tail => countChange(money - head, coins) + countChange(money, tail)
    }
  }
}

