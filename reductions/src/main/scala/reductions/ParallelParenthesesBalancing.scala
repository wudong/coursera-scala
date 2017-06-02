package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {

    def doloop(chars: Array[Char], numOpens: Int) : Boolean = {
      if (chars.isEmpty) numOpens == 0
      else{
        val cur = chars.head
        val n =
          if (cur=='(') numOpens + 1
          else if (cur==')') numOpens -1
          else numOpens

        if (n >=0) doloop(chars.tail, n)
        else false
      }
    }

    doloop(chars, 0)

  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    //find unmatched, '(' and ')' in the current given range.
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      def helper (chars: List[Char], acc: (Int, Int)): (Int, Int) = {
        chars match {
          case Nil => acc
          case '(' :: tail => helper(tail, (acc._1 + 1, acc._2))
          case ')' :: tail => {
            if (acc._1 > 0) helper(tail, (acc._1 -1, acc._2))
            else helper(tail, (acc._1, acc._2 +1))
          }
          case x :: tail => helper(tail, acc)
        }
      }
      helper(chars.slice(idx, until).toList, (arg1, arg2))
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = (from + until) / 2
        val (l, r) = parallel (reduce(from, mid), reduce(mid, until))
        val matched = scala.math.min(l._1, r._1)
        (l._1 + r._1 - matched, l._2 + r._2 - matched)
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
