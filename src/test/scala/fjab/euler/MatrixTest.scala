package fjab.euler

import fjab.euler.Matrix._
import org.scalatest.FunSuite

class MatrixTest extends FunSuite{

  val matrix: Array[Array[Int]] =
    Array(
      Array(1,2,3,4),
      Array(5,6,7,8),
      Array(9,0,1,2),
      Array(3,4,5,6)
    )

  test("rows"){
    println(matrix.map(_.mkString(",")).mkString("\n"))
    assert(matrix(0).mkString(",") == "1,2,3,4")
    assert(matrix(1).mkString(",") == "5,6,7,8")
    assert(matrix(2).mkString(",") == "9,0,1,2")
    assert(matrix(3).mkString(",") == "3,4,5,6")
  }

  test("columns"){
    println(columns(matrix).map(_.mkString(",")).mkString("\n"))
    assert(columns(matrix)(0).mkString(",") == "1,5,9,3")
    assert(columns(matrix)(1).mkString(",") == "2,6,0,4")
    assert(columns(matrix)(2).mkString(",") == "3,7,1,5")
    assert(columns(matrix)(3).mkString(",") == "4,8,2,6")
  }

  test("lowerRightWardsDiagonal"){
    println(lowerRightWardsDiagonal(matrix).map(_.mkString(",")).mkString("\n"))
    assert(lowerRightWardsDiagonal(matrix)(0).mkString(",") == "5,0,5")
    assert(lowerRightWardsDiagonal(matrix)(1).mkString(",") == "9,4")
    assert(lowerRightWardsDiagonal(matrix)(2).mkString(",") == "3")
  }

  test("upperRightWardsDiagonal"){
    println(upperRightWardsDiagonal(matrix).map(_.mkString(",")).mkString("\n"))
    assert(upperRightWardsDiagonal(matrix)(0).mkString(",") == "2,7,2")
    assert(upperRightWardsDiagonal(matrix)(1).mkString(",") == "3,8")
    assert(upperRightWardsDiagonal(matrix)(2).mkString(",") == "4")
  }

  test("lowerLeftWardsDiagonal"){
    println(lowerLeftWardsDiagonal(matrix).map(_.mkString(",")).mkString("\n"))
    assert(lowerLeftWardsDiagonal(matrix)(0).mkString(",") == "8,1,4")
    assert(lowerLeftWardsDiagonal(matrix)(1).mkString(",") == "2,5")
    assert(lowerLeftWardsDiagonal(matrix)(2).mkString(",") == "6")
  }

  test("upperLeftWardsDiagonal"){
    println(upperLeftWardsDiagonal(matrix).map(_.mkString(",")).mkString("\n"))
    assert(upperLeftWardsDiagonal(matrix)(0).mkString(",") == "3,6,9")
    assert(upperLeftWardsDiagonal(matrix)(1).mkString(",") == "2,5")
    assert(upperLeftWardsDiagonal(matrix)(2).mkString(",") == "1")
  }

  test("mainRightWardsDiagonal"){
    assert(mainRightWardsDiagonal(matrix).mkString(",") == "1,6,1,6")
  }

  test("mainLeftWardsDiagonal"){
    assert(mainLeftWardsDiagonal(matrix).mkString(",") == "4,7,0,3")
  }

}
