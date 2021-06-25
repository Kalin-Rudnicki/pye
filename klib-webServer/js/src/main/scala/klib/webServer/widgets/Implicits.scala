package klib.webServer.widgets

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._

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

}
object Implicits extends Implicits
