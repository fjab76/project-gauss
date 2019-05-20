package fjab.euler.graphs.impl

import fjab.euler.graphs.Coordinate

import scala.collection.mutable.ListBuffer

/**
  * Created by franciscoalvarez on 11/06/2017.
  *
  * Knight's tour problem, https://en.wikipedia.org/wiki/Knight%27s_tour
  * A knight's tour is a sequence of moves of a knight on a chessboard such that the knight visits
  * every square only once
  */
class KnightsTourMutable(boardSize: (Int,Int)) extends ChessKnightFiniteMutable(null)(boardSize){

  def findTour(from: Coordinate) = super.findPath(List(List(from)))


  override def addNeighbours(verticesToExplore: ListBuffer[Path], neighbours: List[Path]) =
    verticesToExplore.prependAll(neighbours) //depth-first search

  override def isSolution(path: Path): Boolean = path.length == x * y


}

