package fjab.euler

import scala.annotation.tailrec
import scala.math.BigInt

object Fibonacci {

  def fibonacciNumbersGenerator: Stream[BigInt] = {
    def f(x: BigInt, y: BigInt): Stream[BigInt] = x #:: f(y, x+y)
    1 #:: f(1, 1 + 1)
  }

  //=========================
  //====== LISTS ============
  //=========================

  def firstNFibonacciTerms(n: Int) = {
    @tailrec
    def f(xs: List[Int]): List[Int] =
      if(xs.length < n) f((xs(0)+xs(1)) :: xs)
      else xs

    f(List(1,1)).reverse
  }

  def fibonacciTermsLessThan(n: Int) = {
    @tailrec
    def f(xs: List[Int]): List[Int] = {
      val next = xs(0) + xs(1)
      if (next < n) f(next :: xs)
      else xs
    }

    f(List(1,1)).reverse
  }

  //========================
  //======= STREAMS ========
  //========================

  def firstNFibonacciTermsWithStreams(n: Int) = {

    def f(x: BigInt, y: BigInt): Stream[BigInt] = x #:: f(y, x+y)
    f(1,1) take n
  }

  def fibonacciTermsLessThanWithStreams(n: Int) = {

    def f(x: BigInt, y: BigInt): Stream[BigInt] = {
      if(x < n) x #:: f(y, x + y)
      else Stream()
    }
    f(1,1)
  }

}
