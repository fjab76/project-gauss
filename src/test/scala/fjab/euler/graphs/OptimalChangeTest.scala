package fjab.euler.graphs

import fjab.euler.graphs.impl.OptimalChangeImmutable
import org.scalatest.FunSuite


class OptimalChangeTest extends FunSuite {


  test("change of 0 with set of coins (1,4,6)"){
    assert(new OptimalChangeImmutable(List(1,4,6))(0).optimalChange() == List())
  }

  test("change of 2 with set of coins (1,4,6)"){
    assert(new OptimalChangeImmutable(List(1,4,6))(2).optimalChange() == List(1,1))
  }

  test("change of 8 with set of coins (1,4,6)"){
    assert(new OptimalChangeImmutable(List(1,4,6))(8).optimalChange() == List(4,4))
  }

}
