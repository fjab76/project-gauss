package fjab.euler.graphs

/**
  * Given an infinite chessboard and a knight, calculate the shortest path to a given square
  *
  * The implementation of the method neighbours represent the possible moves of a knight making this an example
  * of undirected cyclic graph. Therefore it is crucial that the implementation of the method isVertexEligibleForPath
  * does not let a path visit the same vertex twice
 */
class ChessKnightInfinite(target: Coordinate) extends GraphTraversal[Coordinate]{

  //knight moves
  override val moves: List[Coordinate] = List((2,1), (1,2), (-1,2), (-2,1), (-2,-1), (-1,-2), (1,-2), (2,-1))

  def findShortestPathFrom(from: Coordinate): Path = findPath(List(List(from)))

  override def neighbours(coordinate: Coordinate): List[Coordinate] = moves.map( coordinate + _)


  /**
   * The nature of the problem requires a breadth-first search in order to find the shortest path
   */
  override def addNeighbours(verticesToExplore: List[Path], neighbours: List[Path]): List[Path] =
    verticesToExplore ++ neighbours

  override def isSolution(path: Path): Boolean = path.head == target

  /**
    * Let's avoid an infinite loop by not visiting previously visited vertices in the present path
    */
  override def isVertexEligibleForPath(vertex: Coordinate, path: Path): Boolean = !path.contains(vertex)
}
