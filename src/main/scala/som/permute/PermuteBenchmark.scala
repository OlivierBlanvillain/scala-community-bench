package permute

import scala.Predef.intWrapper
import scala.Predef.augmentString
import scala.{Int, Boolean, Unit, Array, Any}
import java.lang.String
import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class PermuteBenchmark extends communitybench.Benchmark {
  @Benchmark
  def run(): Any = {
    val size     = input.toInt
    val permIter = (0 until size).toList.permutations

    var count = 0
    while (permIter.hasNext) {
      permIter.next()
      count += 1
    }
    count
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
object PermuteBenchmark {
  def main(args: Array[String]): Unit =
    new PermuteBenchmark().batchRun(args)
}
