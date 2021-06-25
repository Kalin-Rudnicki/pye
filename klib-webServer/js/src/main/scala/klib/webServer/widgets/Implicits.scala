package klib.webServer.widgets

import scala.language.implicitConversions

import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.typeclass.{Applicative, DecodeString}
import klib.fp.types._
import klib.webServer.CSS

trait Implicits {

  implicit class StringWidgetBuilderOps[S](widget: Widget.Builder[String, S]) {

    def decode[T: DecodeString]: Widget.Builder[T, S] =
      widget.flatMapValue(implicitly[DecodeString[T]].decode)

  }

  implicit class MaybeStringWidgetBuilderOps[S](widget: Widget.Builder[Maybe[String], S]) {

    def decode[T: DecodeString]: Widget.Builder[Maybe[T], S] =
      widget.flatMapValue {
        case Some(str) =>
          implicitly[DecodeString[T]].decode(str).map(_.some)
        case None =>
          None.pure[?]
      }

  }

  implicit class MaybeWidgetBuilderOps[T, S](widget: Widget.Builder[Maybe[T], S]) {

    def required(errorMsg: String = "Required field is empty"): Widget.Builder[T, S] =
      widget.flatMapValue(_.toEA(Message(errorMsg)))

  }

  implicit def builderApplicative[S]: Applicative[Widget.Builder.Projection[S]#P] =
    Widget.Builder.builderApplicative

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
object Implicits extends Implicits
