package fjab.euler.graphs

import fjab.euler.graphs.impl.ChessKnightInfiniteImmutable
import org.scalatest.FunSuite

class ChessKnightInfiniteTest extends FunSuite {


  test("should be List((0,0)) for path (0,0)->(0,0)") {
    assert(new ChessKnightInfiniteImmutable((0,0)).findShortestPathFrom((0,0)) == List((0,0)))
  }

  test("should be List((0,0), (2,1), (3,3)) for path (0,0)->(3,3)") {
      assert(new ChessKnightInfiniteImmutable((3,3)).findShortestPathFrom((0,0)) == List((0,0), (2,1), (3,3)))
    }

  test("should be List((1, 2), (3,3), (5,4)) for path (1,2)->(5,4)") {
      assert(new ChessKnightInfiniteImmutable((5,4)).findShortestPathFrom((1,2)) == List((1, 2), (3,3), (5,4)))
    }

  test("should be List() for path (0,0)->(8,1)") {
      assert(new ChessKnightInfiniteImmutable((8,1)).findShortestPathFrom((0,0)) == List((0,0), (2,1), (4,2), (5,4), (6,2), (8,1)))
    }

}
