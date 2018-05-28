package fjab.euler

/**
  * Created by franciscoalvarez on 05/07/2017.
  */
package object graphs {

  type Coordinate = (Int, Int)

  implicit class RichTuple2(coordinate: Coordinate){
    def +(other: Coordinate) = (coordinate._1 + other._1, coordinate._2 + other._2)
    def -(other: Coordinate) = (coordinate._1 - other._1, coordinate._2 - other._2)
  }

}
