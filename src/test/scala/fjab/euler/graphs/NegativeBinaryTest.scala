package fjab.euler.graphs

import fjab.euler.graphs.impl.{NegativeBinary2Mutable, NegativeBinary2Immutable, NegativeBinaryMutable, NegativeBinaryImmutable}
import org.scalatest.FunSuite


class NegativeBinaryTest extends FunSuite {


  test("0 == {0}"){
    val number = 0
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == List(0))
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == new NegativeBinary2Immutable(number).findShortestBinaryRepresentation())
  }

  test("1 == {1}"){
    val number = 1
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == List(1))
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == new NegativeBinary2Immutable(number).findShortestBinaryRepresentation())
  }

  test("2 == {0,1,1}"){
    assert(new NegativeBinaryImmutable(2).findShortestBinaryRepresentation() == List(0,1,1))
  }

  test("-2 == {0,1}"){
    val number = -2
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == List(0,1))
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == new NegativeBinary2Immutable(number).findShortestBinaryRepresentation())
  }


  test("-9 == {1,1,0,1}"){
    val number = -9
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == List(1,1,0,1))
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == new NegativeBinary2Immutable(number).findShortestBinaryRepresentation())
  }

  test("9 == {1,0,0,1,1}"){
    val number = 9
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == List(1,0,0,1,1))
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == new NegativeBinary2Immutable(number).findShortestBinaryRepresentation())
  }

  test("-23 == {1,0,0,1,1,1}"){
    val number = -23
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == List(1,0,0,1,1,1))
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == new NegativeBinary2Immutable(number).findShortestBinaryRepresentation())
  }

  test("23 == {1,1,0,1,0,1,1}"){
    val number = 23
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == List(1,1,0,1,0,1,1))
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == new NegativeBinary2Immutable(number).findShortestBinaryRepresentation())
  }


  test("NegativeBinary performance"){//50,000 = 2m 17s, 100,000 = stopped after 18m
    val number = 100000
    assert(new NegativeBinaryImmutable(number).findShortestBinaryRepresentation() == List(0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1))
  }

  test("NegativeBinaryBuffer performance"){//50,000 = 500ms, 100,000 = 1s 800ms
  val number = 100000
    println(new NegativeBinaryMutable(number).findShortestBinaryRepresentation())
  }

  test("NegativeBinary2 performance"){//50,000 = 51s, 100,000 = stopped after 6m
    val number = 100000
    assert(new NegativeBinary2Immutable(number).findShortestBinaryRepresentation() == List(0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1))
  }

  test("NegativeBinary2Buffer performance"){//50,000 = 285ms , 100,000 = 597ms
  val number = 100000
    println(new NegativeBinary2Mutable(number).findShortestBinaryRepresentation())
  }
}
