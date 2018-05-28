package fjab.euler.graphs

class ChessKnightFinite(to: Coordinate)(boardSize: Tuple2[Int,Int]) extends ChessKnightInfinite(to){

  val x = boardSize._1
  val y = boardSize._2

  override def neighbours(coordinate: Coordinate): List[Coordinate] =
    super.neighbours(coordinate).filter{ case (v, w) => v >= 0 && v < x && w >=0 && w<y }
}
