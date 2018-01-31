package fjab.euler

import org.scalatest.FunSuite
import fjab.euler.Fibonacci._

class FibonacciTest extends FunSuite{

  test("first N fibonacci terms"){
    assert(firstNFibonacciTerms(10) == List(1,1,2,3,5,8,13,21,34,55))
  }

  test("fibonacci terms less than N"){
    assert(fibonacciTermsLessThan(50) == List(1,1,2,3,5,8,13,21,34))
  }

  test("first N fibonacci terms with streams"){
    assert(firstNFibonacciTermsWithStreams(10).toList == List(1,1,2,3,5,8,13,21,34,55))
  }

  test("fibonacci terms less than N with streams"){
    assert(fibonacciTermsLessThanWithStreams(50).toList == List(1,1,2,3,5,8,13,21,34))
  }

}
