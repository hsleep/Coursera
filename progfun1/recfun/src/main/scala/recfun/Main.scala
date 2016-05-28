package recfun

import scala.annotation.tailrec

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
  def pascal(c: Int, r: Int): Int = c match {
    case x if x < 0 || x > r => 0
    case 0 | `r` => 1
    case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def code(ch: Char): Int = ch match {
      case '(' => 1
      case ')' => -1
      case _ => 0
    }
    @tailrec
    def loop(chLs: List[Char], acc: Int = 0): Int = chLs match {
      case head::tail if acc >= 0 => loop(tail, acc + code(head))
      case _ => acc
    }
    loop(chars) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, coins: List[Int], acc: Int = 0): Int = {
      money match {
        case 0 => acc + 1
        case x if x < coins.head => acc + countChange(money, coins.tail)
        case _ => loop(money - coins.head, coins, acc + countChange(money, coins.tail))
      }
    }
    if (money == 0 || coins.isEmpty) 0
    else {
      loop(money, coins)
    }
  }
}
