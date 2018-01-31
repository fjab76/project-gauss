package fjab.euler

import org.scalatest.{FlatSpecLike, FunSpec, FunSpecLike, FunSuite}

class Test extends FunSuite{

  test("decimal representation"){
    assert(decimalRepresentation(234).reverse === List(2,3,4))
  }

  test("count digits benchmark"){
    val t = System.currentTimeMillis()
    (1 to 100000000).map(countDigits(_))
    println(s"${(-t + System.currentTimeMillis())} ms")

    val tt = System.currentTimeMillis()
    (1 to 100000000).map(countDigitsTailRecursive(_))
    println(s"${(-tt + System.currentTimeMillis())} ms")
  }

  test("rotate elements of sequence"){
    assert(rotateLeft(List(1,2,3,4,5),2) === List(3,4,5,1,2))
  }

  test("Collatz series"){
    assert(collatzSeries(10) === List(10, 5, 16, 8, 4, 2, 1))
    //collatzSequenceGenerator(10) take(150) foreach println
  }
}

class TestGroups extends FunSpec{
  describe("A list of 8 elements"){
    describe("when divided in 1 group"){
      it("should result in 1 group with all the elements"){
        assert(groups(8,1) == List((0,0,7)))
      }
    }
    describe("when divided in 2 groups"){
      it("should result in 2 groups with 4 elements each"){
        assert(groups(8,2) == List((0,0,3), (1,4,7)))
      }
    }
    describe("when divided in 3 groups"){
      it("should result in 2 groups with 3 elements and 1 with 2 elements"){
        assert(groups(8,3) == List((0,0,2), (1,3,5), (2,6,7)))
      }
    }
  }

  describe("A list of 1 element"){
    describe("when divided in 1 group"){
      it("should result in 1 group with all the elements"){
        assert(groups(1,1) == List((0,0,0)))
      }
    }
    describe("when divided in 2 groups"){
      it("should throw an Exception"){
        assertThrows[IllegalArgumentException]{
          groups(1,2)
        }
      }
    }
  }
}
