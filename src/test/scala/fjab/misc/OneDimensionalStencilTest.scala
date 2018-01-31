package fjab.misc

import fjab.misc.OneDimensionalStencil._
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.FunSuite

class OneDimensionalStencilTest extends FunSuite {

  implicit val doubleArrayEquality = new Equality[Array[Double]]{
    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.001)
    override def areEqual(a: Array[Double], b: Any): Boolean = {
      b match {
        case brr: Array[Double] => (for(j <- a.indices) yield a(j) === brr(j)).reduceLeft(_ && _)
        case _ => false
      }
    }
  }

  test("seq"){
    val iterations = 10
    val result = seq(iterations, Array(1,2,2,1))
    assert(result === Array(1.0,1.0,1.0,1.0))
    //println(seq(iterations, Array(1,2,2,1)).mkString(","))
  }

  test("seq vs parWithForkJoin"){
    val iterations = 10
    val array: Array[Double] = Array(1,2,2,1)
    assert(seq(iterations, array).mkString(",") == parWithForkJoin(iterations, array).mkString(","))
  }

  test("seq vs parWithLatch"){
    val iterations = 10
    val array: Array[Double] = Array(1,2,2,1)
    assert(seq(iterations, array).mkString(",") == parWithLatch(iterations, array).mkString(","))
  }

  test("seq vs parWithCyclicBarrier"){
    val iterations = 10
    val array: Array[Double] = Array(1,2,2,1)
    assert(seq(iterations, array).mkString(",") == parWithCyclicBarrier(iterations, array).mkString(","))
  }

  test("seq vs parWithPhasers"){
    val iterations = 10
    val array = Array(1.0,2,2,1)
    assert(seq(iterations, array).mkString(",") == parWithPhasers(iterations, array).mkString(","))
  }

  test("seq vs parWithCyclicBarrierAndGroups"){
    val iterations = 10
    val array: Array[Double] = Array(1,2,2,2,2,2,2,1)
    assert(seq(iterations, array).mkString(",") == parWithCyclicBarrierAndGroups(iterations, 3, array).mkString(","))
  }

  test("seq vs parWithPhasersAndGroups"){
    val iterations = 10
    val array = Array(1.0,2,2,2,2,2,2,1)
    assert(seq(iterations, array).mkString(",") == parWithPhasersAndGroups(iterations, 3, array).mkString(","))
  }

  test("seq vs parWithPhasersAndGroupsOptimised"){
    val iterations = 10
    val array = Array(1.0,2,2,2,2,2,2,1)
    assert(seq(iterations, array).mkString(",") == parWithFuzzyBarrier(iterations, 3, array).mkString(","))
  }

  test("seq vs barrier benchmark"){
    val repetitions = 10
    val n = 2000000
    val array = Array.fill(n)(2.0)
    array(0) = 1.0
    array(n-1) = 1.0
    val t = System.currentTimeMillis()
    val r1 = seq(repetitions, array).mkString(",")
    println(-t + System.currentTimeMillis())

    val tt = System.currentTimeMillis()
    val r2 = parWithFuzzyBarrier(repetitions,4,array).mkString(",")
    println(-tt + System.currentTimeMillis())

    assert(r1==r2)
  }

  test("2DimStencil"){
    println("result => " + twoDimAvg(10, Array(Array(1.0,1.0,1.0,1.0), Array(1.0,2.0,2.0,1.0), Array(1.0,2.0,2.0,1.0), Array(1.0,1.0,1.0,1.0))).map(_.mkString(",")).mkString("\n"))
  }

}
