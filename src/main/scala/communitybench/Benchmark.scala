package communitybench

import scala.{Any, Array, Unit, Int, Long}
import java.lang.{String, System}
import scala.Predef.assert
import scala.Predef.augmentString

abstract class Benchmark {
  def run(input: String): Any

  final def main(args: Array[String]): Unit = {
    assert(
      args.length == 3,
      "need to provide a number of iterations, input and expected output (got: " + args.length + ")")
    val iterations = args(0).toInt
    val input      = args(1)
    val output     = args(2)

    dump(loop(input, output, iterations))
  }

  final def dump(times: Array[Long]): Unit = {
    var i = 0
    while (i < times.length) {
      System.out.println(times(i))
      i += 1
    }
  }

  final def loop(input: String,
                 output: String,
                 iterations: Int): Array[Long] = {
    var i     = 0
    val times = new Array[Long](iterations)

    while (i < iterations) {
      val start  = System.nanoTime()
      val result = run(input)
      val end    = System.nanoTime()

      if (result.toString != output) {
        throw new java.lang.Exception(
          "validation failed: expected `" + output + "` got `" + result + "`")
      }

      times(i) = end - start
      i = i + 1
    }

    times
  }
}
