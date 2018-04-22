package fjab

import scala.collection.mutable
import scala.math.BigInt

package object collatz {

  /**
    * Generator of Collatz sequence, https://en.wikipedia.org/wiki/Collatz_conjecture.
    *
    * Collatz sequence is defined as follows: start with any positive integer n.
    * Then each term is obtained from the previous term as follows:
    * - if the previous term is even, the next term is one half the previous term.
    * - otherwise, the next term is 3 times the previous term plus 1
    *
    * The conjecture is that no matter what value of n, the sequence will always reach 1 and will end up
    * in an infinite cycle: 1,4,2,1,4,2,1...
    *
    */
  def collatzSequenceGenerator(n: BigInt): Stream[BigInt] = (n/2)*2 match{
    case `n` => n #:: collatzSequenceGenerator(n/2)
    case _ => n #:: collatzSequenceGenerator(3*n+1)
  }

  /**
    * Similarly to Collatz sequence, there are other sequences that will always end up in an infinite loop
    *
    * For instance, if Collatz sequence is modified to substract 1 instead of adding, any integer n will end up
    * in one of the following loops:
    *
    * - 1, 2, 1
    * - 5, 14, 7, 20, 10, 5
    * - 17, 50, 25, 74, 37, 110, 55, 164, 82, 41, 122, 61, 182, 91, 272, 136, 68, 34, 17
    *
    * Therefore, positive integers can be partitioned in 3 different groups according to the loop they
    * belong to.
    *
    * In mathematical jargon, 3*n-1 defines an equivalence relation and the aforementioned loops are
    * equivalence classes (https://en.wikipedia.org/wiki/Equivalence_relation)
    *
    * If each equivalence class is denoted by its minimum value, then the set of equivalence classes is: 1,5,17.
    *
    * Borrowing Physics terminology, we can think of a positive integer n as an atom that decays to one of the stable
    * values (1,5,17) through a sequence of transformations. For instance, given the number 3, its decay sequence is:
    * 3 -> 8 -> 4 -> 2 -> 1
    *
    */
  def `3*n-1`(n: BigInt): Stream[BigInt] = (n/2)*2 match{
    case `n` => n #:: `3*n-1`(n/2)
    case _ => n #:: `3*n-1`(3*n-1)
  }


  /**
    * The quotient set is the set of all possible equivalence classes by the equivalence relation ~
    * (in mathematical jargon, an equivalence relation is represented by ~)
    *
    * This method calculates the quotient set for a Collantz-like equivalence relation.
    *
    * VERY IMPORTANT: a Collantz-like equivalence relation MUST have a term after which
    * all elements repeat periodically. Otherwise, this method will enter an infinite loop.
    *
    * @param limit Upper limit of the range of positive integers used to calculate the equivalence classes
    * @param `~` Collantz-like equivalence relation
    * @return Set of equivalence classes
    */
  def quotientSet(limit: Int, `~`: BigInt => Stream[BigInt]): Set[List[BigInt]] ={

    val equivalenceClasses = mutable.Set[List[BigInt]]()
    val integersAlreadyComputed = mutable.Set[BigInt]()

    for(i <- 1 to limit){
      if(!integersAlreadyComputed.contains(i)) {
        val iter = (`~`)(i).iterator

        /**
          * Iterates over the sequence until a repeat value is found
          */
        def loop(m: BigInt, s: List[BigInt]): List[BigInt] = if (s.contains(m)) m :: s else loop(iter.next(), m :: s)

        val list = loop(iter.next(), Nil)
        val repeatValue = list.head

        val equivalenceClass = list.slice(0, list.indexOf(repeatValue, 1) + 1).reverse
        if(!integersAlreadyComputed.contains(repeatValue)) equivalenceClasses += equivalenceClass
        integersAlreadyComputed ++= equivalenceClass
      }
    }
    equivalenceClasses.toSet
  }


  /**
    * The projection of ~ is the function that maps elements into their respective equivalence classes by ~
    *
    * In this method, each equivalence class is represented by the minimum value of each sequence of elements that
    * repeat periodically
    *
    * @param n Element to be mapped to its equivalence class
    * @param `~` Collantz-like equivalence relation
    * @param equivalenceClasses Sequence of equivalence classes by ~
    * @return Equivalence class
    */
  def projection(n: BigInt, `~`: BigInt => Stream[BigInt], equivalenceClasses: Seq[BigInt]): BigInt = {

    val iter = (`~`)(n).iterator

    def loop(m: BigInt):BigInt = if (equivalenceClasses.contains(m)) m else loop(iter.next())

    loop(iter.next())
  }

}
