package fjab.euler

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.BigInt

object Primes {

  def isPrime(x: BigInt): Boolean = (2 to scala.math.sqrt(x.doubleValue()).toInt).forall(x%_ > 0)

  /**
    * Find prime numbers in given list
    */
  def eratosthenesSieve(l: List[Int]): List[Int] = {
    assert(l.head > 1)
    l match {
      case Nil => Nil
      case _ :: Nil => l
      case x :: xs => x :: eratosthenesSieve(xs.filter(_ % x > 0))
    }
  }

  def primeNumbersGenerator: Stream[BigInt] = {
    def oddNumbers(n: BigInt): Stream[BigInt] = n #:: oddNumbers(n + 2)
    2 #:: oddNumbers(3).filter(isPrime)
  }

  def primesLessThan(n: Int): List[Int] = {
    def isPrime(x: Int): Boolean = (2 to scala.math.sqrt(x).toInt).forall(x%_ > 0)
    (2 :: (3 until n by 2).toList).filter(isPrime)
  }

  def primesLessThan_optimised(n: Int): List[Int] = {

    val primes = mutable.ListBuffer(2)
    for(x <- 3 until n by 2){
      if(primes.filter(_ <= scala.math.sqrt(x).toInt).forall(x%_ > 0))
        primes += x
    }
    primes.toList
  }

  def firstFactorOf(i: BigInt): BigInt = {
    val iter = primeNumbersGenerator.iterator
    @tailrec
    def f(prime: BigInt): BigInt = if(i%prime == 0) prime else f(iter.next())
    f(iter.next())
  }

  def factorsOf(n: BigInt): List[BigInt] = {
    @tailrec
    def f(m: BigInt, factors: List[BigInt]): List[BigInt] = {
      val x = firstFactorOf(m)
      val quotient = m / x
      if (quotient > 1)
        f(quotient, x :: factors)
      else
        x :: factors
    }

    f(n, Nil).reverse
  }

  /**
    * @param n
    * @return number of divisors of n
    */
  def numDivisors(n: BigInt): Int = {
    val squareRoot = scala.math.sqrt(n.doubleValue())
    val x = (1 until squareRoot.toInt+1).filter(n%_ == 0).length
    if(squareRoot.toInt == squareRoot) 2*x-1
    else 2*x
  }

  /**
    * Given the representation of a number as product of primes
    *
    * n = p^a*q^b*r^c
    *
    * the number of divisors can be obtained as (a+1)*(b+1)*(c+1)
    */
  def numDivisors(factorRepresentation: List[BigInt]): Int = {
    val exponents: List[Int] = factorRepresentation.groupBy(n => n).values.toList.map(_.length)
    exponents.map(_ + 1).product
  }

  /**
    * Euclidean algorithm to calculate the greatest common divisor of 2 numbers x and y
    *
    * The Euclidean algorithm is based on the principle that the greatest common divisor of two numbers does not change
    * if the larger number is replaced by its remainder when divided by the smaller of the two
    * (https://en.wikipedia.org/wiki/Euclidean_algorithm)
    *
    * @param x x must be equal or greater than y
    * @param y
    * @return
    */
  @tailrec
  def euclideanAlgorithm(x: Int, y: Int): Int = y match{
    case 0 => x
    case _ => euclideanAlgorithm(y, x%y)
  }

  def primesSet(n: Int): Set[BigInt] = primeNumbersGenerator take(n) toSet
  def primesList(n: Int): List[BigInt] = primeNumbersGenerator take(n) toList
}
