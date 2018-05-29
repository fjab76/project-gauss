package fjab.euler.graphs

class ChessKnightFinite(target: Coordinate)(boardSize: (Int,Int)) extends ChessKnightInfinite(target){

  val x = boardSize._1
  val y = boardSize._2

  override def neighbours(coordinate: Coordinate): List[Coordinate] =
    super.neighbours(coordinate).filter{ case (v, w) => v >= 0 && v < x && w >=0 && w<y }
}
