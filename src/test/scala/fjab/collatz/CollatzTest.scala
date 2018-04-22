package fjab.collatz

import org.scalatest.FunSuite

class CollatzTest extends FunSuite{

  test("first 5 elements of Collatz sequence for number 5"){
    assert(collatzSequenceGenerator(5).take(5).toList == List(5, 16, 8, 4, 2))
  }

  test("first 5 elements of 3*n-1 sequence for number 5"){
    assert(`3*n-1`(5).take(5).toList == List(5, 14, 7, 20, 10))
  }

  test("quotient set by Collatz with upper bound 100"){
    val equivalenceClasses = quotientSet(100, collatzSequenceGenerator)
    assert(equivalenceClasses.size == 1)
    assert(equivalenceClasses.contains(List(1, 4, 2, 1)))
  }

  test("quotient set by 3*n-1 with upper bound 1000"){
    val equivalenceClasses = quotientSet(1000, `3*n-1`)
    assert(equivalenceClasses.size == 3)
    assert(equivalenceClasses.contains(List(1, 2, 1)))
    assert(equivalenceClasses.contains(List(5, 14, 7, 20, 10, 5)))
    assert(equivalenceClasses.contains(List(17, 50, 25, 74, 37, 110, 55, 164, 82, 41, 122, 61, 182, 91, 272, 136, 68, 34, 17)))
  }


  test("projection of 6 by 3*n-1"){
    assert(projection(6, `3*n-1`, List(1,5,17)) == 1)
  }

  //=====================================================================================================

  test("elements of 3*n-1 sequence"){
    assert(`3*n-1`(13).take(30).toList == List(5, 14, 7, 20, 10))
  }

  test("decay path of elements of equivalence class 1 by 3*n-1 in the range 1-100"){
    val ec = BigDecimal(1)
    1 to 100 filter(projection(_, `3*n-1`, List(1,5,17)) == ec) map(`3*n-1`(_)) foreach(s => println(s.takeWhile(_ != ec) :+ ec toList))
  }

  test("decay path of elements of equivalence class 5 by 3*n-1 in the range 1-100"){
    val ec = BigDecimal(5)
    1 to 100 filter(projection(_, `3*n-1`, List(1,5,17)) == ec) map(`3*n-1`(_)) foreach(s => println(s.takeWhile(_ != ec) :+ ec toList))
  }

  test("decay path of elements of equivalence class 17 by 3*n-1 in the range 1-100"){
    val ec = BigDecimal(17)
    1 to 100 filter(projection(_, `3*n-1`, List(1,5,17)) == ec) map(`3*n-1`(_)) foreach(s => println(s.takeWhile(_ != ec) :+ ec toList))
  }

  test("size of the equivalence classes by 3*n-1 in different ranges"){
    def ecSize(upperLimit: Int): Unit = 1 to upperLimit groupBy(projection(_, `3*n-1`, List(1,5,17))) map{case (k,v) => (k, v.size)} foreach{case (k,v) => println(s"$k: $v elements, ${v*1.0/upperLimit}")}

    ecSize(100)
    println("===============================")
    ecSize(1000)
    println("===============================")
    ecSize(10000)
    println("===============================")
    ecSize(100000)
    println("===============================")
    ecSize(1000000)
  }


}
