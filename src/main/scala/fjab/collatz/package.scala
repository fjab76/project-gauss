package fjab

import scala.math.BigInt

package object collatz {

  def collatzSequenceGenerator(n: BigInt): Stream[BigInt] = (n/2)*2 match{
    case `n` => n #:: collatzSequenceGenerator(n/2)
    case _ => n #:: collatzSequenceGenerator(3*n+1)
  }

  def `3*n-1`(n: BigInt): Stream[BigInt] = (n/2)*2 match{
    case `n` => n #:: `3*n-1`(n/2)
    case _ => n #:: `3*n-1`(3*n-1)
  }

}
