/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * Based on nbody.java and adapted basde on the SOM version.
 */
package nbody

import scala.{Double, Boolean, Unit, Array, Any}
import java.lang.String
import scala.Predef.augmentString
import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class NbodyBenchmark extends communitybench.Benchmark {
  @Benchmark
  def run(): Any = {
    val system = new NBodySystem()
    val n      = input.toInt

    var i = 0
    while (i < n) {
      system.advance(0.01)
      i += 1
    }

    system.energy() == -0.1690859889909308
  }
}
object NbodyBenchmark {
  def main(args: Array[String]): Unit =
    new NbodyBenchmark().batchRun(args)
}
