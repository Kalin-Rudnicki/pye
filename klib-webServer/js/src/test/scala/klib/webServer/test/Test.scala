package klib.webServer.test

import scala.reflect.ClassTag

import monocle.syntax.all._
import monocle.macros.GenLens
import monocle._
import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._

object Test {
  def main(args: Array[String]): Unit = {

    final case class Person(
        firstName: String,
        lastName: String,
        age: Int,
        friendsNames: Array[String],
    ) {

      override def toString: String =
        s"Person($firstName $lastName ($age), ${friendsNames.mkString("[", ",", "]")})"

    }

    final case class Couple(
        man: Person,
        woman: Person,
    )

    def arrayLens[T: ClassTag](idx: Int): Lens[Array[T], T] =
      Lens[Array[T], T](_(idx)) { t => at =>
        val copied = at.clone
        copied(idx) = t
        copied
      }

    val couple0 =
      Couple(
        Person("Kalin", "Rudnicki", 23, Array.empty),
        Person("Jennifer", "Lawrence", 30, Array.empty),
      )

    val couple1 =
      couple0.focus(_.woman.age).modify(_ => 22)

    val personsFriends =
      GenLens[Person](_.friendsNames)

    println(couple1)

    val couple2 = couple1.focus(_.man).andThen(personsFriends).modify(_.appended("Test"))
    println
    println(couple1)
    println(couple2)

    val couple3 = couple2.focus(_.man).andThen(personsFriends).andThen(arrayLens[String](0)).replace("Jeffrey")
    println
    println(couple1)
    println(couple2)
    println(couple3)

  }
}
