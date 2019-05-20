package fjab.euler.graphs

import fjab.euler.graphs.impl.{KnightsTourMutable, KnightsTourImmutable}
import org.scalatest.FunSpec

/**
 * Created by franciscoalvarez on 04/06/2017.
 */
class KnightsTourTest extends FunSpec {

  describe("The solution for a 4x4 board") {
    it("should be an empty list for position (0,0)") {
      assert(new KnightsTourImmutable(4, 4).findTour((0, 0)) == List())
    }
  }

  describe("8x8 board"){
    val obj = new KnightsTourImmutable(8,8)
    //18 seconds
    it("initial position (0,0)") {
      val result = obj.findTour((0,0))
      assert(result.size == 8*8)
      assert(result == List((0,0), (2,1), (4,2), (6,3), (7,5), (6,7), (4,6), (2,7), (0,6), (1,4), (3,5), (5,6), (7,7), (6,5), (5,7), (3,6), (1,7), (0,5), (2,6), (4,7), (5,5), (7,6), (6,4), (4,5), (6,6), (5,4), (3,3), (2,5), (3,7), (1,6), (0,4), (1,2), (2,4), (0,3), (1,1), (3,0), (2,2), (1,0), (0,2), (2,3), (4,4), (3,2), (4,0), (6,1), (7,3), (5,2), (7,1), (5,0), (3,1), (4,3), (5,1), (7,0), (6,2), (7,4), (5,3), (7,2), (6,0), (4,1), (2,0), (0,1), (1,3), (3,4), (1,5), (0,7)))
    }

    //takes too long with basic implementation
    ignore("initial position (1,1)") {
      val result = obj.findTour((1,1))
      assert(result.size == 64)
      assert(result == List((1,1), (3,2), (5,3), (7,4), (6,6), (4,7), (2,6), (0,7), (1,5), (3,6), (5,7), (4,5), (3,7), (1,6), (0,4), (2,5), (4,6), (6,7), (5,5), (7,6), (6,4), (5,6), (7,7), (6,5), (4,4), (2,3), (3,5), (2,7), (0,6), (1,4), (0,2), (1,0), (3,1), (5,2), (7,3), (6,1), (4,0), (2,1), (0,0), (1,2), (3,3), (5,4), (7,5), (6,3), (7,1), (5,0), (6,2), (4,3), (2,4), (0,5), (1,3), (3,4), (4,2), (3,0), (1,1), (0,3), (2,2), (0,1), (2,0), (4,1), (6,0), (7,2), (5,1), (7,0)))
    }

    //takes too long
    ignore("with Buffer") {
      val result = new KnightsTourMutable(8,8).findTour(1,1)
      assert(result.size == 64)
      assert(result == List((1,1), (3,2), (5,3), (7,4), (6,6), (4,7), (2,6), (0,7), (1,5), (3,6), (5,7), (4,5), (3,7), (1,6), (0,4), (2,5), (4,6), (6,7), (5,5), (7,6), (6,4), (5,6), (7,7), (6,5), (4,4), (2,3), (3,5), (2,7), (0,6), (1,4), (0,2), (1,0), (3,1), (5,2), (7,3), (6,1), (4,0), (2,1), (0,0), (1,2), (3,3), (5,4), (7,5), (6,3), (7,1), (5,0), (6,2), (4,3), (2,4), (0,5), (1,3), (3,4), (4,2), (3,0), (1,1), (0,3), (2,2), (0,1), (2,0), (4,1), (6,0), (7,2), (5,1), (7,0)))
    }
  }




}
