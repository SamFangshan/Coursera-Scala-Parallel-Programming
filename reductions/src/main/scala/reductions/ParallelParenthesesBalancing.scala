package reductions

import scala.annotation._
import org.scalameter._

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
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var count = 0
    for (char <- chars) {
      if (char == '(') {
        count += 1
      } else if (char == ')') {
        count -= 1
      }
      if (count < 0) {
        false
      }
    }
    if (count == 0) {
      true
    } else {
      false
    }
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, until: Int, arg1: Int, arg2: Int): Int = {
      var i = from
      var count1 = arg1
      var count2 = arg2
      while (i < until) {
        if (chars(i) == '(') {
          count1 += 1
        } else if (chars(i) == ')') {
          count2 += 1
        }
        i += 1
      }
      count1 - count2
    }

    def reduce(from: Int, until: Int): Int = {
      if ((until - from) / 2 >= threshold) {
        val (res1, res2) = parallel(traverse(from, threshold, 0, 0), traverse(threshold, until, 0, 0))
        res1 + res2
      } else {
        traverse(from, until, 0, 0)
      }
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
