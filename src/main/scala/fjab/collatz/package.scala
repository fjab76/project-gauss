package fjab


import scala.collection.mutable
import scala.math.BigInt

package object collatz {

  abstract class CollatzLikeSequenceGenerator(val upperBound: BigInt) {

    /**
      * Some implementation examples of this function are encapsulated in
      * the object CollatzLikeSequenceGenerator
      */
    def `~`(n: BigInt): Stream[BigInt]

    /**
      *  The quotient set is the set of all possible equivalence classes by the equivalence relation ~
      * (in mathematical jargon, an equivalence relation is represented by ~)
      *
      * This method calculates the quotient set for a Collantz-like equivalence relation.
      *
      * VERY IMPORTANT: a Collantz-like equivalence relation ~ MUST have a term after which
      * all elements repeat periodically. Otherwise, this method will enter an infinite loop.
      *
      * This algorithm ensures that the equivalence classes constructed are the canonical ones (the ones starting
      * with the minimum value of the cycle)
      */
    lazy val quotientSet: Set[List[BigInt]] = {

      val equivalenceClasses = mutable.Set[List[BigInt]]()
      val integersAlreadyComputed = mutable.Set[BigInt]()

      for(i <- BigInt(1) to upperBound){
        if(!integersAlreadyComputed.contains(i)) {
          val iter = (`~`)(i).iterator

          /**
            * Iterates over the sequence until a repeat value or a previously computed integer is found
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

    /**
      * The canonical representation of a equivalence class can also be identified by its minimum value
      */
    lazy val quotientSetRepresentatives = {
      quotientSet.map(_.head)
    }

    /**
      * The projection of ~ is the function that maps elements into their respective equivalence classes by ~
      *
      * @param n Value whose equivalence class is to be calculated
      * @return Value representing the equivalence class
      */
    def projection(n: BigInt): BigInt = {

      if(n > upperBound) throw new IllegalArgumentException(s"$n must be equal or less than $upperBound")

      val iter = (`~`)(n).iterator
      def loop(m: BigInt):BigInt = if (quotientSetRepresentatives.contains(m)) m else loop(iter.next())
      loop(iter.next())
    }

    /**
      * Sequence of elements from n to the representative value (the minimum one) of its equivalence class
      */
    def decayPath(n: BigInt): List[BigInt] = {

      if(n > upperBound) throw new IllegalArgumentException(s"$n must be equal or less than $upperBound")

      val iter = (`~`)(n).iterator
      def loop(m: BigInt, s: List[BigInt]): List[BigInt] = if (quotientSetRepresentatives.contains(m)) m :: s else loop(iter.next(), m :: s)
      loop(iter.next(), Nil).reverse
    }

    /**
      * Returns the entry point of n in its cycle. The entry point is the first element of the cycle in the decay path
      */
    def entryPoint(n: BigInt): BigInt = {
      if(n > upperBound) throw new IllegalArgumentException(s"$n must be equal or less than $upperBound")

      val iter = (`~`)(n).iterator
      def loop(m: BigInt):BigInt = if (quotientSet.filter(_.contains(m)).size == 1) m else loop(iter.next())
      loop(iter.next())
    }

    /**
      * Classifies the integers from 1 to n according to its entry point in the cycle target
      * Returns a map between each element of the cycle target and the integers that have that element as entry point
      */
    def entryPointsClassification(n: BigInt, target: BigInt): Map[BigInt, Seq[BigInt]] = {
      BigInt(1) to n filter(projection(_) == target) groupBy(entryPoint)
    }

  }

  /**
    * Object containing implementations of some Collatz-like sequences
    */
  object CollatzLikeSequenceGenerator{

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
      * in one of the following cycles:
      *
      * - 1, 2, 1
      * - 5, 14, 7, 20, 10, 5
      * - 17, 50, 25, 74, 37, 110, 55, 164, 82, 41, 122, 61, 182, 91, 272, 136, 68, 34, 17
      *
      * Given that these cycles repeat infinitely, 1,2,1 is equivalent to 2,1,2. The above representation,
      * starting with the minimum value, can be taken as the canonical representation.
      * For brevity, they can be represented as <1>, <5> and <17> respectively.
      *
      * The sequence of numbers through which any positive integer "decays" into one of the loops is called decay path.
      * For instance, given the number 3, its decay path is: 3 -> 8 -> 4 -> 2 -> 1
      *
      * Therefore, positive integers can be partitioned in 3 different groups according to its cycle
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

    def `5*n-1`(n: BigInt): Stream[BigInt] = (n/2)*2 match{
      case `n` => n #:: `5*n-1`(n/2)
      case _ => n #:: `5*n-1`(5*n-1)
    }
  }

  implicit class Path(n: Int)(implicit clsg: CollatzLikeSequenceGenerator){

    if(n > clsg.upperBound) throw new IllegalArgumentException(s"$n must be equal or less than ${clsg.upperBound}")

    /**
      * The path from n to m (n <=> m) is the sequence of numbers that connect n with m throughout their decay path.
      * Cases:
      * - if the equivalence classes of n and m are different, then n and m are not connected, e.g. 6 <=> 40 = {}
      * - if m is in the decay path of n, then the connecting path is a subset of n's decay path, e.g. 16 <=> 2 = {16,8,4,2}
      * - if n is in the decay path of m, then the connecting path is a subset of m's decay path going in reverse, e.g. 16 <=> 11 = {16,32,11}
      * - if the decay paths of n and m overlap, then the connecting path is the concatenation of the subset of their decay path
      * that goes up to the intersection point, e.g. 11 <=> 6 = {11,32,16,8,3,6}, where 8 is the intersection point of the decay paths
      * of 11 and 6
      *
      * @param m
      * @return
      */
    def <=>(m: BigInt): List[BigInt] = {
      val projectionN = clsg.projection(n)
      val projectionM = clsg.projection(m)

      if(projectionN != projectionM) List()
      else {

        val path1 = pathWhenTheSecondIsInTheDecayPathOfTheFirst(n,m)
        if(path1 == Nil){
          val path2 = pathWhenTheSecondIsInTheDecayPathOfTheFirst(n,m)
          if(path2 == Nil) pathWhenTheirDecayPathsOverlap(n,m)
          else path2.reverse
        }
        else path1
      }
    }

    def pathWhenTheSecondIsInTheDecayPathOfTheFirst(n: BigInt, m: BigInt): List[BigInt] = {
      val decayPathN = clsg.decayPath(n)
      val idx = decayPathN.indexOf(m)
      if(idx >= 0) decayPathN.slice(0,idx + 1)
      else Nil
    }

    def pathWhenTheirDecayPathsOverlap(n: Int, m: BigInt): List[BigInt] = {
      val decayPathN = clsg.decayPath(n)
      val decayPathM = clsg.decayPath(m)

      val intersectionPoint = decayPathN.intersect(decayPathM).head
      decayPathN.slice(0, decayPathN.indexOf(intersectionPoint) + 1) ++
        decayPathM.slice(0, decayPathM.indexOf(intersectionPoint)).reverse
    }
  }

}
