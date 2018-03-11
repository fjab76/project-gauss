package fjab.mycats.monoid

import cats.Monoid
import org.scalatest.FunSuite


object BooleanMonoidInstances{

  val andMonoid: Monoid[Boolean] = new Monoid[Boolean]{
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  val orMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  //exclusive or (xor)
  /**
    * 0 0 -> 0
    * 1 0 -> 1
    * 0 1 -> 1
    * 1 1 -> 0
    */
  val xorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = x ^ y
  }

  //exclusive nor (!xor)
  /**
    * 0 0 -> 1
    * 1 0 -> 0
    * 0 1 -> 0
    * 1 1 -> 1
    */
  val xnorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = !(x ^ y)
  }
}

class Test extends FunSuite{
  test("xor"){
    import BooleanMonoidInstances.xorMonoid
    assert(!xorMonoid.combine(false,false))
    assert(xorMonoid.combine(true,false))
    assert(xorMonoid.combine(false,true))
    assert(!xorMonoid.combine(true,true))
  }

  test("xnor"){
    import BooleanMonoidInstances.xnorMonoid
    assert(xnorMonoid.combine(false,false))
    assert(!xnorMonoid.combine(true,false))
    assert(!xnorMonoid.combine(false,true))
    assert(xnorMonoid.combine(true,true))
  }

  test("set"){

    implicit def setUnionMonoid[A]: Monoid[Set[A]] =
      new Monoid[Set[A]] {
        def combine(a: Set[A], b: Set[A]) = a union b
        def empty = Set.empty[A]
      }

    val intSetMonoid = Monoid[Set[Int]]

    assert(intSetMonoid.combine(Set(1, 2), Set(2, 3)) == Set(1,2,3))
  }

  test("adding all the things"){

    import cats.instances.int._ // for Monoid
    import cats.instances.option._ // for Monoid
    import cats.syntax.semigroup._ // for |+|

    case class Order(totalCost: Double, quantity: Double)

    implicit val orderMonoid: Monoid[Order] = new Monoid[Order]{

      override def empty: Order = Order(0,0)
      override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    }

    def addInts(items: List[Int]): Int = {
      items.foldLeft(Monoid[Int].empty)(_ |+| _)
    }

    def addOptionalInts(items: List[Option[Int]]): Option[Int] = {
      items.foldLeft(Monoid[Option[Int]].empty)(_ |+| _)
    }

    def add[A: Monoid](items: List[A]): A = {
      items.foldLeft(Monoid[A].empty)(_ |+| _)
    }

    assert(addInts(List(1,2,3)) == 6)

    assert(addOptionalInts(List(Some(1),Some(2),Some(3))) == Some(6))
    assert(addOptionalInts(List(Some(1),Some(2),None)) == Some(3))

    assert(add(List(1,2,3)) == 6)
    assert(add(List(Option(1),Option(2),Option(3))) == Some(6))
    assert(add(List(Some(1),Some(2),None)) == Some(3))

    assert(add(List(Order(2.0, 1.0), Order(1.5, 2.0))) == Order(3.5, 3.0))


  }
}
