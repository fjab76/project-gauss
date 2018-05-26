package fjab.collatz

import org.scalatest.FunSuite

import scala.math.BigInt

class Generator5nMinus1Test extends FunSuite{

  class Generator5nMinus1(upperBound: BigInt) extends CollatzLikeSequenceGenerator(upperBound){
    override def `~`(n: BigInt): Stream[BigInt] = CollatzLikeSequenceGenerator.`5*n-1`(n)
  }

  test("decay paths"){
    new Generator5nMinus1(1).`~`(2).take(10).foreach(println)
  }

  test("quotientSet in the range 1-8"){
    val equivalenceClasses = new Generator5nMinus1(8).quotientSet
    assert(equivalenceClasses.size == 1)
    assert(equivalenceClasses.contains(List(1, 4, 2, 1)))
  }

}
