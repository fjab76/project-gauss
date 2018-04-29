package fjab.collatz

import org.scalatest.FunSuite

import scala.math.BigInt

class CollatzTest extends FunSuite{

  val collatzGenerator = new CollatzLikeSequenceGenerator(100){
    override def `~`(n: BigInt): Stream[BigInt] = CollatzLikeSequenceGenerator.collatzSequenceGenerator(n)
  }

  val `3*n-1Generator` = new CollatzLikeSequenceGenerator(10000000) {
    override def `~`(n: BigInt): Stream[BigInt] = CollatzLikeSequenceGenerator.`3*n-1`(n)

    override lazy val quotientSet: Set[List[BigInt]] = Set(
      List(1, 2, 1),
      List(5, 14, 7, 20, 10, 5),
      List(17, 50, 25, 74, 37, 110, 55, 164, 82, 41, 122, 61, 182, 91, 272, 136, 68, 34, 17)
    )
  }


  test("decay path of 5 by collatz"){
    assert(collatzGenerator.decayPath(5) === List(5, 16, 8, 4, 2, 1))
  }

  test("decay path of 3 by 3*n-1"){
    assert(`3*n-1Generator`.decayPath(3) === List(3,8,4,2,1))
  }

  test("quotient set by Collatz in the range 1-100"){
    val equivalenceClasses = collatzGenerator.quotientSet
    assert(equivalenceClasses.size == 1)
    assert(equivalenceClasses.contains(List(1, 4, 2, 1)))
  }

  test("quotient set by 3*n-1 in the range 1-100"){
    val equivalenceClasses = `3*n-1Generator`.quotientSet
    assert(equivalenceClasses.size == 3)
    assert(equivalenceClasses.contains(List(1, 2, 1)))
    assert(equivalenceClasses.contains(List(5, 14, 7, 20, 10, 5)))
    assert(equivalenceClasses.contains(List(17, 50, 25, 74, 37, 110, 55, 164, 82, 41, 122, 61, 182, 91, 272, 136, 68, 34, 17)))
  }


  test("projection of 6 by 3*n-1"){
    assert(`3*n-1Generator`.projection(6) == 1)
  }

  //=====================================================================================================

  test("elements of 3*n-1 sequence"){
    `3*n-1`(13).take(30).toList
  }

  test("decay path of elements of <1> by 3*n-1 in the range 1-100"){
    val ec = BigDecimal(1)
    1 to 100 filter(`3*n-1Generator`.projection(_) == ec) map(`3*n-1Generator`.decayPath(_)) foreach println
  }

  test("decay path of elements of <5> by 3*n-1 in the range 1-100"){
    val ec = BigDecimal(5)
    1 to 100 filter(`3*n-1Generator`.projection(_) == ec) map(`3*n-1Generator`.decayPath(_)) foreach println
  }


  test("classification of decay path of elements of <1> by 3*n-1 in the range 1-100"){
    val ec = BigDecimal(1)
    val groups = BigInt(1) to 10000 filter(`3*n-1Generator`.projection(_) == ec) groupBy(`3*n-1Generator`.entryPoint)
    val total: Int = groups.map{case (x,y) => y.size}.sum
    groups.foreach{case(x,y) => println(s"$x = ${y.size.toDouble/total}")}
  }

  test("classification of decay path of elements of <5> by 3*n-1 in the range 1-100"){
    val ec = BigInt(5)
    val groups = BigInt(1) to 10000000 groupBy(`3*n-1Generator`.entryPoint(_, ec))
    val total: Int = groups.filterNot{case (x,y) => x == -1}.map{case (x,y) => y.size}.sum
    groups.filterNot{case (x,y) => x == -1}.foreach{case(x,y) => println(s"$x = ${y.size.toDouble/total}")}
  }//10^6 -> 50s, 45s , 10^7 -> 10m 19s, 8m 22s

  test("classification of decay path of elements of <17> by 3*n-1 in the range 1-100"){
    val ec = BigDecimal(17)
    val groups = BigInt(1) to 10000 filter(`3*n-1Generator`.projection(_) == ec) groupBy(`3*n-1Generator`.entryPoint)
    val total: Int = groups.map{case (x,y) => y.size}.sum
    groups.foreach{case(x,y) => println(s"$x = ${y.size.toDouble/total}")}
  }

  test("decay path of elements of <17> by 3*n-1 in the range 1-100"){
    val ec = BigDecimal(17)
    1 to 100 filter(`3*n-1Generator`.projection(_) == ec) map(`3*n-1Generator`.decayPath(_)) foreach println
  }


  test("case: there is no path connecting the numbers, e.g. 6 and 40"){
    implicit val clsg = `3*n-1Generator`
    assert((6 <=> 40) == List())
  }

  test("case: the second number is in the decay path of the other, e.g. 16 => 2 = 16,8,4,2"){
    implicit val clsg = `3*n-1Generator`
    assert((16 <=> 2) === List(16,8,4,2))
  }

  test("case: the first number is in the decay path of the other, e.g. 16 => 2 = 16,8,4,2"){
    implicit val clsg = `3*n-1Generator`
    assert((16 <=> 11) === List(16,32,11))
  }

  test("case 3: the decay paths overlap, e.g. 11 => 6 = 11,32,16,8,3,6"){
    implicit val clsg = `3*n-1Generator`
    assert((11 <=> 6) === List(11,32,16,8,3,6))
    assert((6 <=> 11) === List(6,3,8,16,32,11))
  }

  test("entry point of 19 is 14") {
    assert(`3*n-1Generator`.entryPoint(19) === 14)
  }

  test("entry point of 27 is 20"){
    assert(`3*n-1Generator`.entryPoint(27) === 20)
  }


  test("quotientSet of 10^6"){
    val `3*n-1Generator` = new CollatzLikeSequenceGenerator(1000000) {
      override def `~`(n: BigInt): Stream[BigInt] = CollatzLikeSequenceGenerator.`3*n-1`(n)
    }
    `3*n-1Generator`.quotientSet foreach(println) //1m 13s //42s // 2s
  }

  test("quotientSet of 2*10^6"){
    val `3*n-1Generator` = new CollatzLikeSequenceGenerator(2000000) {
      override def `~`(n: BigInt): Stream[BigInt] = CollatzLikeSequenceGenerator.`3*n-1`(n)
    }
    `3*n-1Generator`.quotientSet foreach(println) //2m 47s // 1m 35s // 10s
  }

  test("quotientSet of 10^7"){
    val `3*n-1Generator` = new CollatzLikeSequenceGenerator(10000000) {
      override def `~`(n: BigInt): Stream[BigInt] = CollatzLikeSequenceGenerator.`3*n-1`(n)
    }
    `3*n-1Generator`.quotientSet foreach(println) //18m 5s // 10m 11s // 1m
  }


}
