package communitybench

import scala.{Any, Array, Unit, Int, Long}
import java.lang.{String, System}
import scala.Predef.assert
import scala.Predef.augmentString
import org.openjdk.jmh.annotations._

abstract class Benchmark {
  var input: String = _

  /** One iteration of this benchmark. */
  def run(): Any

  // JMH setup ----------------------------------------------------------------

  @Setup(Level.Trial)
  def jmhSetup(): Unit = {
    // Loads the content of ./input/${this.getClass} into this.input
    import java.nio.charset.StandardCharsets.UTF_8
    import java.nio.file.{Files, Paths}
    val jmhName     = this.getClass().getName().toString
    val className   = jmhName.substring(0, jmhName.indexOf('.')) + jmhName.substring(jmhName.lastIndexOf('.'))
    val inputFile   = "input/" + className.substring(0, className.length - "_jmhType".length)
    val inputString = new String(Files.readAllBytes(Paths.get(inputFile)), UTF_8).trim
    this.input = inputString
  }

  // Non-JMH setup ------------------------------------------------------------

  /** Runs several interation of the benchmark and dumps output to stdout.
    * To be called from main methods.
    */
  final def batchRun(args: Array[String]): Unit = {
    assert(
      args.length == 4,
      "4 arguments expected: number of batches, batch size, input and expected output")
    val batches   = args(0).toInt
    val batchSize = args(1).toInt
    this.input    = args(2)
    val output    = args(3)

    dump(loop(batches, batchSize, output))
  }

  final def dump(times: Array[Long]): Unit = {
    var i = 0
    while (i < times.length) {
      System.out.println(times(i))
      i += 1
    }
  }

  final def loop(batches: Int,
                 batchSize: Int,
                 output: String): Array[Long] = {
    assert(batches >= 1)
    assert(batchSize >= 1)

    var i       = 0
    val times   = new Array[Long](batches)
    val results = new Array[Any](batchSize)

    while (i < batches) {
      val start = System.nanoTime()

      var j = 0
      while (j < batchSize) {
        results(j) = run()
        j += 1
      }

      val end = System.nanoTime()

      j = 0
      while (j < batchSize) {
        val result = results(j)
        if (result.toString != output) {
          throw new java.lang.Exception(
            "validation failed: expected `" + output + "` got `" + result + "`")
        }
        results(j) = null
        j += 1
      }

      times(i) = end - start
      i += 1
    }

    times
  }
}
