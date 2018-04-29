package fjab

import fjab.collatz.{`3*n-1`, collatzSequenceGenerator}

import scala.collection.mutable
import scala.math.BigInt

package object collatz {

  abstract class CollatzLikeSequenceGenerator(val upperBound: BigInt) {

    def `~`(n: BigInt): Stream[BigInt]

    /**
      * Algorithm ensures that the equivalence classes constructed are the canonical ones
      * @return
      */
    lazy val quotientSet: Set[List[BigInt]] = {

      val equivalenceClasses = mutable.Set[List[BigInt]]()
      val integersAlreadyComputed = mutable.Set[BigInt]()

      for(i <- BigInt(1) to upperBound){
        if(!integersAlreadyComputed.contains(i)) {
          val iter = (`~`)(i).iterator

          /**
            * Iterates over the sequence until a repeat value is found
            */
          def loop(m: BigInt, s: List[BigInt]): List[BigInt] =
            if (s.contains(m)) m :: s
            else if(integersAlreadyComputed.contains(m)) -1 :: s
            else loop(iter.next(), m :: s)

          val list = loop(iter.next(), Nil)

          if(list.head != -1) {
            val repeatValue = list.head
            val equivalenceClass = list.slice(0, list.indexOf(repeatValue, 1) + 1).reverse
            if (!integersAlreadyComputed.contains(repeatValue)) equivalenceClasses += equivalenceClass
          }
          integersAlreadyComputed ++= list
        }
      }
      equivalenceClasses.toSet
    }

    lazy val canonicalRepresentation = {
      quotientSet.map(_.head)
    }

    def projection(n: BigInt): BigInt = {

      if(n > upperBound) throw new IllegalArgumentException(s"$n must be equal or less than $upperBound")

      val iter = (`~`)(n).iterator
      def loop(m: BigInt):BigInt = if (canonicalRepresentation.contains(m)) m else loop(iter.next())
      loop(iter.next())
    }

    def decayPath(n: BigInt): List[BigInt] = {

      if(n > upperBound) throw new IllegalArgumentException(s"$n must be equal or less than $upperBound")

      val iter = (`~`)(n).iterator
      def loop(m: BigInt, s: List[BigInt]): List[BigInt] = if (canonicalRepresentation.contains(m)) m :: s else loop(iter.next(), m :: s)
      loop(iter.next(), Nil).reverse
    }

    def entryPoint(n: BigInt): BigInt = {
      if(n > upperBound) throw new IllegalArgumentException(s"$n must be equal or less than $upperBound")

      val iter = (`~`)(n).iterator
      def loop(m: BigInt):BigInt = if (quotientSet.filter(_.contains(m)).size == 1) m else loop(iter.next())
      loop(iter.next())
    }

    def entryPoint(n: BigInt, targetEc: BigInt): BigInt = {
      if (n > upperBound) throw new IllegalArgumentException(s"$n must be equal or less than $upperBound")

      val iter = (`~`) (n).iterator

      def loop(m: BigInt): BigInt = {
        val ec = quotientSet.filter(_.contains(m))
        if (ec.size == 1) {
          if (ec.head.head == targetEc) m
          else -1
        }
        else
          loop(iter.next())
      }

      loop(iter.next())
    }
  }

  object CollatzLikeSequenceGenerator{
    def collatzSequenceGenerator(n: BigInt): Stream[BigInt] = (n/2)*2 match{
      case `n` => n #:: collatzSequenceGenerator(n/2)
      case _ => n #:: collatzSequenceGenerator(3*n+1)
    }

    def `3*n-1`(n: BigInt): Stream[BigInt] = (n/2)*2 match{
      case `n` => n #:: `3*n-1`(n/2)
      case _ => n #:: `3*n-1`(3*n-1)
    }
  }

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
    * Given that these loops repeat infinitely, 1,2,1 is equivalent to 2,1,2 and so on. The above representation,
    * starting with the minimum value, is the canonical representation. For brevity, they can be represented as
    * <1>, <5> and <17> respectively.
    *
    * The sequence of numbers through which any positive integer decays into one of the loops is called decay path.
    * For instance, given the number 3, its decay sequence is: 3 -> 8 -> 4 -> 2 -> 1
    *
    * Therefore, positive integers can be partitioned in 3 different groups according to the loop they decay in
    *
    * In mathematical jargon, 3*n-1 defines an equivalence relation (represented by ~) and the loops are
    * equivalence classes (https://en.wikipedia.org/wiki/Equivalence_relation).
    *
    * The set of all possible equivalence classes by the equivalence relation ~ is called quotient set,
    * e.g. /~ = List(1,5,17)
    *
    */
  def `3*n-1`(n: BigInt): Stream[BigInt] = (n/2)*2 match{
    case `n` => n #:: `3*n-1`(n/2)
    case _ => n #:: `3*n-1`(3*n-1)
  }

  implicit class BigIntExtensions(n: BigInt) {

    /**
      * Calculates the decay path of n induced by the equivalence relation ~ over the set of positive integers
      * in the range 1 to n.
      * The quotient set (set of equivalence classes) by ~ is passed as a parameter to make the calculation more
      * performant (as opposed to have it calculated within the method)
      *
      * @param `~`
      * @param `n/~`
      * @return List representing the decay path of n: starts with n and ends with the minimum value of the loop
      */
    def decayPath()(implicit `~`: BigInt => Stream[BigInt], `n/~`: List[BigInt]): List[BigInt] = {
      val iter = (`~`)(n).iterator
      def loop(m: BigInt, s: List[BigInt]): List[BigInt] = if (`n/~`.contains(m)) m :: s else loop(iter.next(), m :: s)
      loop(iter.next(), Nil).reverse
    }
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
  def quotientSet(limit: BigInt, `~`: BigInt => Stream[BigInt]): Set[List[BigInt]] ={

    val equivalenceClasses = mutable.Set[List[BigInt]]()
    val integersAlreadyComputed = mutable.Set[BigInt]()

    for(i <- BigInt(1) to limit){
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

  def quotientSetWithSize(limit: BigInt, `~`: BigInt => Stream[BigInt]): Map[List[BigInt],BigInt] ={

    val equivalenceClasses = mutable.Map[List[BigInt], BigInt]()
    val integersAlreadyComputed = mutable.Set[BigInt]()

    for(i <- BigInt(1) to limit){
      if(!integersAlreadyComputed.contains(i)) {
        val iter = (`~`)(i).iterator

        /**
          * Iterates over the sequence until a repeat value is found
          */
        def loop(m: BigInt, s: List[BigInt]): List[BigInt] = if (s.contains(m)) m :: s else loop(iter.next(), m :: s)

        val list = loop(iter.next(), Nil)
        val repeatValue = list.head

        val equivalenceClass = list.slice(0, list.indexOf(repeatValue, 1) + 1).reverse
        if(!integersAlreadyComputed.contains(repeatValue))
          equivalenceClasses += (equivalenceClass -> 1)
        else {
          val canonicalEquivalenceClass = equivalenceClasses.keys.filter(_.contains(repeatValue)).head
          equivalenceClasses(canonicalEquivalenceClass) = equivalenceClasses(canonicalEquivalenceClass) + 1
        }

        integersAlreadyComputed ++= equivalenceClass
      }
      else{
        val canonicalEquivalenceClass = equivalenceClasses.keys.filter(_.contains(i)).head
        equivalenceClasses(canonicalEquivalenceClass) = equivalenceClasses(canonicalEquivalenceClass) + 1
      }
    }

    equivalenceClasses.toMap
  }


  /**
    * The projection of ~ is the function that maps elements into their respective equivalence classes by ~
    *
    * In this method, each equivalence class is represented by the minimum value of each sequence of elements that
    * repeat periodically
    *
    * @param n Element to be mapped to its equivalence class
    * @param x Collantz-like equivalence relation
    * @param y Sequence of equivalence classes by ~
    * @return Equivalence class
    */
  def projection(n: BigInt)(implicit x: BigInt => Stream[BigInt], y: Seq[BigInt]): BigInt = {

    val iter = (x)(n).iterator
    def loop(m: BigInt):BigInt = if (y.contains(m)) m else loop(iter.next())
    loop(iter.next())
  }

  implicit class Path(n: Int)(implicit clsg: CollatzLikeSequenceGenerator){

    if(n > clsg.upperBound) throw new IllegalArgumentException(s"$n must be equal or less than ${clsg.upperBound}")

    def <=>(m: BigInt): List[BigInt] = {
      val projectionN = clsg.projection(n)
      val projectionM = clsg.projection(m)

      if(projectionN != projectionM) List()
      else {
        isTheSecondInThePathOfTheFirst(n,m) match {
          case Nil => isTheSecondInThePathOfTheFirst(m,n) match{
            case Nil => doTheirPathsOverlap(n,m)
            case xs => xs.reverse
          }
          case xs => xs
        }
      }
    }

    def isTheSecondInThePathOfTheFirst(n: BigInt, m: BigInt): List[BigInt] = {
      val decayPathN = clsg.decayPath(n)
      val idx = decayPathN.indexOf(m)
      if(idx >= 0) decayPathN.slice(0,idx + 1)
      else Nil
    }

    def doTheirPathsOverlap(n: Int, m: BigInt): List[BigInt] = {
      val decayPathN = clsg.decayPath(n)
      val decayPathM = clsg.decayPath(m)

      val intersectionPoint = decayPathN.intersect(decayPathM).head
      decayPathN.slice(0, decayPathN.indexOf(intersectionPoint) + 1) ++
        decayPathM.slice(0, decayPathM.indexOf(intersectionPoint)).reverse
    }
  }

}
