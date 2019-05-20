package fjab.euler.graphs.impl

import fjab.euler.graphs.api.SharedKnowledgeGraphTraversal

import scala.collection.mutable.ListBuffer

/**
  *
  * Given an integer, calculate the shortest sequence of bits representing that number in base -2
  * In base -2, the sequence B of N bits represents the number
  * sum{B[i]*(-2).pow(i) for i=0...N-1}
  * The empty sequence represents 0
  *
  * With some imagination, the search for the solution to this problem can be equated to searching for a path in a
  * graph with the following characteristics:
  * - the vertices of this graph are 0s and 1s
  * - from any given vertex, it is possible to move only to a new 0 or a new 1 (new in the sense that those vertices
  * have not yet been visited). Because of this limitation on the allowed moves, there are never repeat vertices in
  * a path and as a result, it is not necessary to implement any extra filter on 'isVertexEligibleForPath'.
  * - another consequence of the previous point is that the graph is infinite.
  * - the succession of 0s and 1s in a path is interpreted as a binary representation
  *
  */
class NegativeBinary2Mutable(number: Int) extends SharedKnowledgeGraphTraversal[(Int, List[Int])]{

  val moves: List[Int] = List(0, 1)

  def findShortestBinaryRepresentation() = findPath(List(List((0, List(0))), List((1, List(1))))).reverse.head._2.reverse

  override def neighbours(vertex: Vertex): List[Vertex] = {
    val value = vertex._1
    val binaryRepresentation = vertex._2
    List((value, moves(0) :: binaryRepresentation), (value + BigInt(-2).pow(binaryRepresentation.size).toInt, moves(1) :: binaryRepresentation))
  }

  def addNeighbours(verticesToExplore: ListBuffer[Path], neighbours: List[Path]) =
    verticesToExplore ++= neighbours //breadth-first search

  override def isSolution(path: Path): Boolean = path.head._1 == number


  /**
    * By construction, a path can never visit the same vertex twice. Therefore, no extra filter is needed.
    */
  override def isVertexEligibleForPath(vertex: Vertex, path: Path): Boolean = true
}

