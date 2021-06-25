package klib.webServer

import scala.language.implicitConversions

import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._

object CSS {

  object Implicits {

    implicit def blockToB[B <: CSS.Block](block: B): CSS.B[B] =
      new CSS.B(false, block)

    implicit def blockToModifier(b: CSS.Block): Modifier =
      `class` := b.classes

    implicit def bToModifier(b: CSS.B[_]): Modifier =
      `class` := b.classes

    implicit def bmToModifier(b: CSS.BM[_]): Modifier =
      `class` := b.classes

    implicit def bemToModifier(b: CSS.BEM[_, _]): Modifier =
      `class` := b.classes

  }

  // =====|  |=====

  private def combine(modifiersOnly: Boolean, block: String, modifiers: List[String]): String =
    List(
      (!modifiersOnly).maybe(block).toList,
      modifiers.map(m => s"$block--$m"),
    ).flatten.mkString(" ")

  final class B[B <: Block] private[webServer] (
      modifiersOnly: Boolean,
      block: B,
  ) {

    def ! : CSS.B[B] =
      new CSS.B(!modifiersOnly, block)

    def classes: String =
      combine(modifiersOnly, block.name, Nil)

    def e[E <: B#Element](elementF: B => E): BEM[B, E] =
      new BEM[B, E](modifiersOnly, block, elementF(block), Nil)

    def __[E <: B#Element](element: E): BEM[B, E] =
      new BEM[B, E](modifiersOnly, block, element, Nil)

    def m(modifierFs: (B => B#Modifier)*): BM[B] =
      new BM[B](modifiersOnly, block, modifierFs.map(_(block)).toList)

    def --(modifiers: B#Modifier*): BM[B] =
      new BM[B](modifiersOnly, block, modifiers.toList)

    def --?(modifierPairs: (Boolean, B#Modifier)*): BM[B] =
      new BM[B](modifiersOnly, block, modifierPairs.toList.filter(_._1).map(_._2))

    override def toString: String =
      classes

  }
  final class BM[B <: Block] private[webServer] (
      modifiersOnly: Boolean,
      block: B,
      modifiers: List[B#Modifier],
  ) {

    def classes: String =
      combine(modifiersOnly, block.name, modifiers.map(_.name))

    def m(modifierFs: (B => B#Modifier)*): BM[B] =
      new BM[B](modifiersOnly, block, this.modifiers ::: modifierFs.map(_(block)).toList)

    def --(modifiers: B#Modifier*): BM[B] =
      new BM[B](modifiersOnly, block, this.modifiers ::: modifiers.toList)

    def --?(modifierPairs: (Boolean, B#Modifier)*): BM[B] =
      new BM[B](modifiersOnly, block, this.modifiers ::: modifierPairs.toList.filter(_._1).map(_._2))

    override def toString: String =
      classes

  }
  final class BEM[B <: Block, E <: B#Element] private[webServer] (
      modifiersOnly: Boolean,
      block: B,
      element: E,
      modifiers: List[E#Modifier],
  ) {

    def classes: String =
      combine(modifiersOnly, s"${block.name}__${element.name}", modifiers.map(_.name))

    def m(modifierFs: (E => E#Modifier)*): BEM[B, E] =
      new BEM[B, E](modifiersOnly, block, element, this.modifiers ::: modifierFs.map(_(element)).toList)

    def --(modifiers: E#Modifier*): BEM[B, E] =
      new BEM[B, E](modifiersOnly, block, element, this.modifiers ::: modifiers.toList)

    def --?(modifierPairs: (Boolean, E#Modifier)*): BEM[B, E] =
      new BEM[B, E](modifiersOnly, block, element, this.modifiers ::: modifierPairs.toList.filter(_._1).map(_._2))

    override def toString: String =
      classes

  }

  // =====|  |=====

  private def calcName(`class`: Class[_]): String =
    `class`.getSimpleName
      .stripPrefix("$")
      .stripSuffix("$")
      .replaceAll("\\$minus", "-")

  abstract class Block private (_name: Maybe[String]) { block =>
    def this() = this(None)
    def this(name: String) = this(name.some)

    private[CSS] val name: String =
      _name.getOrElse(calcName(block.getClass))

    abstract class Element private (_name: Maybe[String]) { element =>
      def this() = this(None)
      def this(name: String) = this(name.some)

      private[CSS] val name: String =
        _name.getOrElse(calcName(element.getClass))

      abstract class Modifier private (_name: Maybe[String]) { element =>
        def this() = this(None)
        def this(name: String) = this(name.some)

        private[CSS] val name: String =
          _name.getOrElse(calcName(element.getClass))

      }

    }

    abstract class Modifier private (_name: Maybe[String]) { element =>
      def this() = this(None)
      def this(name: String) = this(name.some)

      private[CSS] val name: String =
        _name.getOrElse(calcName(element.getClass))

    }

  }

}
