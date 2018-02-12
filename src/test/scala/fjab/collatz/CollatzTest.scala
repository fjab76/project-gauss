package fjab.collatz

import org.scalatest.FunSuite

import scala.collection.mutable
import scala.math.BigInt


class CollatzTest extends FunSuite{

  /**
    * https://en.wikipedia.org/wiki/Equivalence_relation
    * The set of all possible equivalence classes
    * @param limit
    * @return
    */
  def quotientSet(limit: Int): Set[List[BigInt]] ={

    val cycles = mutable.Set[List[BigInt]]()
    val valuesAlreadyComputed = mutable.Set[BigInt]()

    for(i <- List(1,2).toIterator ++ Range(3,limit,2)){
      if(!valuesAlreadyComputed.contains(i)) {
        val iter = `3*n-1`(i).iterator

        def loop(m: BigInt, s: List[BigInt]): List[BigInt] = if (s.contains(m)) m :: s else loop(iter.next(), m :: s)

        val list = loop(iter.next(), Nil)
        val repeatValue = list.head

        val cycle = list.slice(0, list.indexOf(repeatValue, 1) + 1).reverse
        if(!valuesAlreadyComputed.contains(repeatValue)) cycles += cycle
        valuesAlreadyComputed ++= cycle
      }
    }
    cycles.toSet
  }

  /**
    * Function that maps elements into their respective equivalence classes
    * @param n
    * @return
    */
  def projection(n: BigInt) = {
    val equivalenceClasses = List(1,5,17)

    val iter = `3*n-1`(n).iterator

    def loop(m: BigInt):BigInt = if (equivalenceClasses.contains(m)) m else loop(iter.next())

    loop(iter.next())
  }

  def elementsOfEquivalenceClass(ec: Int, limit: Int) = {
    Range.inclusive(1, limit).filter(i => projection(i) == ec).toList
  }

  test("3*n-1"){
    `3*n-1`(53) take 20 foreach println
  }

  test("quotient set"){
    quotientSet(1001).foreach(println)
  }

  test("projection"){
    println(projection(1000))
  }

  test("elementsOfEquivalenceClass"){
    val limit = 60
    println(elementsOfEquivalenceClass(1,limit))//.size/limit.toDouble)
    println(elementsOfEquivalenceClass(5,limit))//.size/limit.toDouble)
    println(elementsOfEquivalenceClass(17,limit))//.size/limit.toDouble)
  }

}
