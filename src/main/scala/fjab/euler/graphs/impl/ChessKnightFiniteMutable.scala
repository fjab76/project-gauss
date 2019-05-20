package fjab.euler.graphs.impl

import fjab.euler.graphs.Coordinate

class ChessKnightFiniteMutable(target: Coordinate)(boardSize: (Int,Int)) extends ChessKnightInfiniteMutable(target){

  val x = boardSize._1
  val y = boardSize._2

  override def neighbours(coordinate: Coordinate): List[Coordinate] =
    super.neighbours(coordinate).filter{ case (v, w) => v >= 0 && v < x && w >=0 && w<y }
}
