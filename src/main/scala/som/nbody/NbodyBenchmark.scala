/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * Based on nbody.java and adapted basde on the SOM version.
 */
package nbody

object NbodyBenchmark extends communitybench.Benchmark {
  def run(input: String): Double = {
    val system = new NBodySystem()
    val n      = input.toInt

    var i = 0
    while (i < n) {
      system.advance(0.01)
      i += 1
    }

    system.energy()
  }
}
