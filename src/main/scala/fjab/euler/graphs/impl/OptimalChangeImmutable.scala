package fjab.euler.graphs.impl

import fjab.euler.graphs.Coin
import fjab.euler.graphs.api.ImmutableGraphTraversal


class OptimalChangeImmutable(coins: List[Coin])(amount: Int) extends ImmutableGraphTraversal[Coin]{

  val moves: List[Coin] = coins

  def optimalChange() = findPath(List(List(0))).tail

  override def neighbours(vertex: Int): List[Int] = moves

  override def addNeighbours(verticesToExplore: List[Path], neighbours: List[Path]) =
    verticesToExplore ++ neighbours //breadth-first search

  override def isSolution(path: Path): Boolean = amount == path.sum

  override def isVertexEligibleForPath(vertex: Int, path: Path): Boolean = (amount - path.sum) >= vertex
}

