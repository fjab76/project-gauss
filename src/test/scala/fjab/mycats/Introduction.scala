package fjab.mycats.introduction

import org.scalatest.FunSuite

/*
    Implementing a type class

    Type classes are a programming pattern originating in Haskell.
    They allow us to extend existing libraries with new funcionality, without using tradicional inheritance,
    and without altering the original library source code.

    There are three important components to the type class pattern:
    1.the type class itself,
    2.instances for particular types,
    3.and the interface methods that we expose to users.


    A type class is an interface or API that represents some functionality we want to implement.
    In Cats a type class is represented by a trait with at least one type parameter

    The instances of a type class provide implementations for the types we care about,
    including types from the Scala standard library and types from our domain model.

    A type class interface is any functionality we expose to users. Interfaces are generic methods
    that accept instances of the type class as implicit parameters.
    There are two common ways of specifying an interface: Interface Objects and Interface Syntax.
*/


final case class Cat(name: String, age: Int, color: String)

trait Printable[A] {
  def format(a: A): String
}

object PrintableInstances{

  implicit val printableString: Printable[String] = (a: String) => a
  implicit val printableInt: Printable[Int] = (a: Int) => a.toString
  implicit val printableCat: Printable[Cat] = (a: Cat) => {
    val name  = Printable.format(a.name)
    val age   = Printable.format(a.age)
    val color = Printable.format(a.color)
    s"$name is a $age year-old $color cat."
  }
}

object Printable{

  def format[A](a: A)(implicit printable: Printable[A]): String = printable.format(a)
  def print[A](a: A)(implicit printable: Printable[A]): Unit = println(printable.format(a))
}

object PrintableSyntax{

  implicit class PrintableOps[A](a: A){
    def format(implicit printable: Printable[A]): String = printable.format(a)
    def print(implicit printable: Printable[A]): Unit = println(printable.format(a))
  }
}

class Test extends FunSuite{

  test("printing cat with Printable"){
    import PrintableInstances._
    import PrintableSyntax._

    Cat("Thor", 4, "red").print
  }

  test("printing cat with Show"){
    import cats.Show
    import cats.instances.int._
    import cats.instances.string._
    import cats.syntax.show._

    implicit val showCat: Show[Cat] = (a: Cat) => {
        val name  = a.name.show
        val age   = a.age.show
        val color = a.color.show
        s"$name is a $age year-old $color cat."
      }


    println(Cat("Thor", 4, "red").show)
  }

  test("comparing cats with Eq"){
    import cats.Eq
    import cats.instances.int._ // for Eq
    import cats.instances.string._
    import cats.instances.option._
    import cats.syntax.eq._
    val eqInt = Eq[Int]
    val eqString = Eq[String]
    assert(eqInt.eqv(123, 123))

    implicit val eqCat: Eq[Cat] = (a: Cat, b: Cat) => eqString.eqv(a.name, b.name) && eqInt.eqv(a.age,b.age) && eqString.eqv(a.color,b.color)

    val cat1 = Cat("Garfield",   38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")

    assert(cat1 =!= cat2)

    val eqCatOption = Eq[Option[Cat]]

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    assert(optionCat1 =!= optionCat2)
  }
}
