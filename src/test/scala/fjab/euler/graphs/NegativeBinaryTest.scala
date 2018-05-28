package fjab.euler.graphs

import org.scalatest.FunSuite


class NegativeBinaryTest extends FunSuite {


  test("0 == {0}"){
    assert(new NegativeBinary(0).findShortestBinaryRepresentation() == List(0))
  }

  test("1 == {1}"){
    assert(new NegativeBinary(1).findShortestBinaryRepresentation() == List(1))
  }

  test("2 == {0,1,1}"){
    assert(new NegativeBinary(2).findShortestBinaryRepresentation() == List(0,1,1))
  }

  test("-2 == {0,1}"){
    assert(new NegativeBinary(-2).findShortestBinaryRepresentation() == List(0,1))
  }


  test("-9 == {1,1,0,1}"){
    assert(new NegativeBinary(-9).findShortestBinaryRepresentation() == List(1,1,0,1))
  }

  test("9 == {1,0,0,1,1}"){
    assert(new NegativeBinary(9).findShortestBinaryRepresentation() == List(1,0,0,1,1))
  }

  test("-23 == {1,0,0,1,1,1}"){
    assert(new NegativeBinary(-23).findShortestBinaryRepresentation() == List(1,0,0,1,1,1))
  }

  test("23 == {1,1,0,1,0,1,1}"){
    assert(new NegativeBinary(23).findShortestBinaryRepresentation() == List(1,1,0,1,0,1,1))
  }


  test("{0,1,1} == 2"){
    assert(new NegativeBinary(2).isSolution(List(1,1,0)))
  }
}
