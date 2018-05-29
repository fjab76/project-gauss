package fjab.euler.graphs


class OptimalChange(coins: List[Coin])(amount: Int) extends GraphTraversal[Int]{

  val moves: List[Coin] = coins

  def optimalChange() = findPath(List(List(0))).tail

  override def neighbours(vertex: Int): List[Int] = moves

  override def addNeighbours(verticesToExplore: List[Path], neighbours: List[Path]): List[Path] =
    verticesToExplore ++ neighbours //breadth-first search

  override def isSolution(path: Path): Boolean = amount == path.sum

  override def isVertexEligibleForPath(vertex: Int, path: Path): Boolean = (amount - path.sum) >= vertex
}

