package permute

import scala.Predef.intWrapper

class PermuteBenchmark {
  val size = 6

  def run(): Int = {
    val permIter = (0 until size).toList.permutations

    var count = 0
    while (permIter.hasNext) {
      permIter.next()
      count += 1
    }
    count
  }

  def check(value: Int): Boolean = {
    value == factorial(size)
  }

  private def factorial(i: Int): Int = {
    var n    = i
    var fact = 1
    while (n > 0) {
      fact *= n
      n -= 1
    }
    fact
  }
}
