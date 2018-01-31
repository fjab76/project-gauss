package fjab.euler.solution

import org.scalameter.{Key, Warmer, config}

object Problem1{

  def linearTimeSolution(num: Int) = (1 until num).filter(n => n%3 == 0 || n%5 == 0).sum
  def linearTimeSolutionPar(num: Int) = (1 until num).par.filter(n => n%3 == 0 || n%5 == 0).sum

  def constantTimeSolution(num: Int) = {
    def arithmeticSum(n: Int) = n * (n + 1) / 2
    def sumOfMultiplesOf(n: Int) = n * arithmeticSum((num -1)/n)
    sumOfMultiplesOf(3) + sumOfMultiplesOf(5) - sumOfMultiplesOf(15)
  }

  def main(args: Array[String]): Unit = {
    val standardConfig = config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 40,
      Key.exec.benchRuns -> 20,
      Key.verbose -> true
    ) withWarmer (new Warmer.Default)

    val n = 10000000

    val seqtime = standardConfig measure {
      linearTimeSolution(n)
    }

    val partime = standardConfig measure {
      linearTimeSolutionPar(n)
    }

    val time = standardConfig measure {
      constantTimeSolution(n)
    }

    println(s"sequencial time: $seqtime ")
    println(s"parallel time: $partime ")
    println(s"arithmetic time: $time ")
  }

}
