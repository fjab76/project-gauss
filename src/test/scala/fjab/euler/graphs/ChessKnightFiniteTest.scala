package fjab.euler.graphs

import org.scalatest.FunSpec

class ChessKnightFiniteTest extends FunSpec {

  describe("The solution for a 8x8 board") {
    it("should be List((0,0)) for path (0,0)->(0,0)") {
      assert(new ChessKnightFinite((0,0))(8,8).findShortestPathFrom((0,0)) == List((0,0)))
    }

    it("should be List((0,0), (2,1), (3,3)) for path (0,0)->(3,3)") {
      assert(new ChessKnightFinite((3,3))(8,8).findShortestPathFrom((0,0)) == List((0,0), (2,1), (3,3)))
    }

    it("should be List((1, 2), (3,3), (5,4)) for path (1,2)->(5,4)") {
      assert(new ChessKnightFinite((5,4))(8,8).findShortestPathFrom((1,2)) == List((1, 2), (3,3), (5,4)))
    }

    it("should be List() for path (0,0)->(8,1)") {
      assert(new ChessKnightFinite((8,1))(8,8).findShortestPathFrom((0,0)) == List())
    }
  }

  describe("The solution for a 16x16 board") {
    //...
    ignore("should be List((0,0), (2,1), (4,2), (6,3), (8,4), (10,5), (12,6), (13,8), (14,10), (15,12), (14,14)) for path (0,0)->(14,14)") {
      assert(new ChessKnightFinite((14,14))(16,16).findShortestPathFrom((0, 0)) == List((0,0), (2,1), (4,2), (6,3), (8,4), (10,5), (12,6), (13,8), (14,10), (15,12), (14,14)))
    }
  }

}
