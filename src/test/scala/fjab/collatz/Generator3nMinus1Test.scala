package fjab.collatz

import org.scalatest.FunSuite

import scala.math.BigInt

class Generator3nMinus1Test extends FunSuite{

  abstract class Generator3nMinus1(upperBound: BigInt) extends CollatzLikeSequenceGenerator(upperBound){
    override def `~`(n: BigInt): Stream[BigInt] = CollatzLikeSequenceGenerator.`3*n-1`(n)
  }

  test("quotientSet in the range 1-100"){
    val equivalenceClasses = new Generator3nMinus1(100){}.quotientSet
    assert(equivalenceClasses.size == 3)
    assert(equivalenceClasses.contains(List(1, 2, 1)))
    assert(equivalenceClasses.contains(List(5, 14, 7, 20, 10, 5)))
    assert(equivalenceClasses.contains(List(17, 50, 25, 74, 37, 110, 55, 164, 82, 41, 122, 61, 182, 91, 272, 136, 68, 34, 17)))
  }

  ignore("quotientSet in the range 1-10^7"){
    new Generator3nMinus1(10000000){}.quotientSet foreach(println) //1 minute
  }

  /**
    * To speed up calculations, this instance defines quotientSet with the values obtained in the previous test
    */
  val `3*n-1Generator precalculated` = new Generator3nMinus1(10000000) {
    override lazy val quotientSet: Set[List[BigInt]] = Set(
      List(1, 2, 1),
      List(5, 14, 7, 20, 10, 5),
      List(17, 50, 25, 74, 37, 110, 55, 164, 82, 41, 122, 61, 182, 91, 272, 136, 68, 34, 17)
    )
  }


  test("decay path of 3"){
    assert(`3*n-1Generator precalculated`.decayPath(6) === List(6,3,8,4,2,1))
  }

  test("projection of 6"){
    assert(`3*n-1Generator precalculated`.projection(6) == 1)
  }

  test("entry point of 19 is 14") {
    assert(`3*n-1Generator precalculated`.entryPoint(19) === 14)
    assert(`3*n-1Generator precalculated`.entryPoint(19, 5).get === 14)
  }

  test("entry point of 27 is 20"){
    assert(`3*n-1Generator precalculated`.entryPoint(27) === 20)
    assert(`3*n-1Generator precalculated`.entryPoint(27, 5).get === 20)
  }

  test("entry point of 27 for equivalence class <1> does not exist"){
    assert(`3*n-1Generator precalculated`.entryPoint(27, 1).isEmpty)
  }

  test("comparison of methods of classification of decay path"){
    val ec = BigInt(17)
    val upperBound = 1000
    assert(`3*n-1Generator precalculated`.entryPointsClassification1(upperBound, ec) == `3*n-1Generator precalculated`.entryPointsClassification2(upperBound, ec))
  }

  test("classification of entry points of <1> in the range 1-1000"){
    val ec = BigInt(1)
    val upperBound = 1000
    val groups = `3*n-1Generator precalculated`.entryPointsClassification1(upperBound, ec)
    val total = groups.map{case (x,y) => y.size}.sum
    groups.foreach{case(x,y) => println(s"$x = ${y.size.toDouble/total}")}
  }

  test("classification of entry points of <5> in the range 1-1000"){
    val ec = BigInt(5)
    val upperBound = 1000
    val groups = `3*n-1Generator precalculated`.entryPointsClassification1(upperBound, ec)
    val total = groups.map{case (x,y) => y.size}.sum
    groups.foreach{case(x,y) => println(s"$x = ${y.size.toDouble/total}")}
  }

  test("classification of entry points of <17> in the range 1-1000"){
    val ec = BigInt(17)
    val upperBound = 1000
    val groups = `3*n-1Generator precalculated`.entryPointsClassification1(upperBound, ec)
    val total = groups.map{case (x,y) => y.size}.sum
    groups.foreach{case(x,y) => println(s"$x = ${y.size.toDouble/total}")}
  }


  test("there is no path connecting the numbers, e.g. 6 and 40"){
    implicit val clsg = `3*n-1Generator precalculated`
    assert((6 <=> 40) == List())
  }

  test("the second number is in the decay path of the other, e.g. 16 => 2 = 16,8,4,2"){
    implicit val clsg = `3*n-1Generator precalculated`
    assert((16 <=> 2) === List(16,8,4,2))
  }

  test("the first number is in the decay path of the other, e.g. 16 => 11 = 16,32,11"){
    implicit val clsg = `3*n-1Generator precalculated`
    assert((16 <=> 11) === List(16,32,11))
  }

  test("the decay paths overlap, e.g. 11 => 6 = 11,32,16,8,3,6"){
    implicit val clsg = `3*n-1Generator precalculated`
    assert((11 <=> 6) === List(11,32,16,8,3,6))
    assert((6 <=> 11) === List(6,3,8,16,32,11))
  }

  test("the decay paths overlap within the final cycle, e.g. 99 => 163 = 11,32,16,8,3,6"){
    implicit val clsg = `3*n-1Generator precalculated`
    assert((99 <=> 163) === List(99,296,148,74,37,110,55,164,82,41,122,244,488,163))
    assert((163 <=> 99) === (99 <=> 163).reverse)
  }





}
