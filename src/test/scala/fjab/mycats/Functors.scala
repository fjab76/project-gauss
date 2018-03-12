package fjab.mycats

import org.scalatest.FunSuite

class Test extends FunSuite{

  test("binary tree functor"){

    import cats.Functor
    import cats.syntax.functor._     // for map

    sealed trait Tree[+A]
    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]

    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value) => Leaf(f(value))
      }
    }

    assert((Branch(Branch(Leaf(1),Leaf(2)), Leaf(3)): Tree[Int]).map(x => (x + 1).toString) == Branch(Branch(Leaf("2"),Leaf("3")), Leaf("4")))

  }
}
