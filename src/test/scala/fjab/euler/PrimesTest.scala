package fjab.euler

import fjab.euler.Primes._
import org.scalatest.FunSuite

class PrimesTest extends FunSuite{

  test("eratosthenesSieve"){
    assert(eratosthenesSieve((2 to 10).toList) == List(2, 3, 5, 7))
  }

  test("primesLessThan"){
    assert(primesLessThan(10) == List(2, 3, 5, 7))
  }

  test("primesLessThan_optimised"){
    assert(primesLessThan_optimised(10) == List(2, 3, 5, 7))
  }

  test("primeNumbersGenerator"){
    assert(primeNumbersGenerator.take(4).toList == List(2, 3, 5, 7))
  }

  test("firstFactorOf"){
    assert(firstFactorOf(1331) == 11)
  }

  test("factorsOf"){
    assert(factorsOf(28) == List(2,2,7))
  }

  test("numDivisors with 2 equals divisors"){
    assert(numDivisors(25) == 3)
  }

  test("numDivisors"){
    assert(numDivisors(10) == 4)
  }

  test("numDivisors based on factor representation"){
    assert(numDivisors(List(2, 5).map(BigInt(_))) == 4)
  }

}
