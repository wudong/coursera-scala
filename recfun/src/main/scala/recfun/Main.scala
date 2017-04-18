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
      c match {
        case 0 => 1
        case x if x ==r => 1
        case _ => pascal( c-1, r-1) + pascal(c, r-1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      println(s"input: ${chars.mkString("")}")
      //it will return the the list start with ')', or Nil if cannot be matched.
      def matchBrace(chars : List[Char], iter: Int) : List[Char] = {
        println( s"Current level ${iter} for matching: ${chars.mkString("")}")
       // val opening=findNextOpening(chars)
        //assume(!opening.isEmpty, "should not be empty.")
        val openingTail = chars.dropWhile( x => x!= '(' && x!= ')')
        assume(!openingTail.isEmpty)
        if (openingTail.head == '(')
          findNextEnding(openingTail.tail, iter)
        else
          Nil
      }

     // def findNextOpening(chars: List[Char]) : List[Char] = chars.dropWhile(_ != '(')

      def findNextEnding(chars: List[Char], iter: Int): List[Char] ={
          val openingTail = chars.dropWhile( x => x!= '(' && x!= ')')
          if (openingTail.isEmpty) Nil
          else {
            if (openingTail.head== '(') {
              val tail = matchBrace(openingTail, iter+1)
              findNextEnding(tail.tail, iter)
            }else {// ')'
              openingTail
            }
          }
      }

      if (chars.isEmpty) true
      else if (chars.dropWhile(x=> x!='(' && x!= ')').isEmpty) true
      else {
        val tail = matchBrace(chars, 1)
        if (tail.isEmpty) false
        else balance(tail.tail)
      }
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      //4 => (1,2)

      def countChangeWithOrderedCoins(money: Int, coins : List[Int]) : Int = {
        money match {
          case x if x < 0 => 0
          case 0 => 1
          case _ => {
            if (coins.isEmpty) 0
            else
              countChangeWithOrderedCoins(money - coins.head, coins) + countChangeWithOrderedCoins(money, coins.tail)
          }
        }
      }

      countChangeWithOrderedCoins(money, coins.sorted)

    }
  }
