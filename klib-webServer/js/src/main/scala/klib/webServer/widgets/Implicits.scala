package klib.webServer.widgets

import klib.Implicits._
import klib.fp.typeclass.DecodeString
import klib.fp.types._

trait Implicits {

  implicit class StringWidgetBuilderOps[S](widget: Widget.Builder[String, S]) {

    def decode[T: DecodeString]: Widget.Builder[T, S] =
      widget.flatMap(implicitly[DecodeString[T]].decode)

  }

  implicit class MaybeStringWidgetBuilderOps[S](widget: Widget.Builder[Maybe[String], S]) {

    def decode[T: DecodeString]: Widget.Builder[Maybe[T], S] =
      widget.flatMap {
        case Some(str) =>
          implicitly[DecodeString[T]].decode(str).map(_.some)
        case None =>
          None.pure[?]
      }

  }

  implicit class MaybeWidgetBuilderOps[T, S](widget: Widget.Builder[Maybe[T], S]) {

    def required(errorMsg: String = "Required field is empty"): Widget.Builder[T, S] =
      widget.flatMap(_.toEA(Message(errorMsg)))

  }

  implicit class WidgetBuilderOps[T, S](widget: Widget.Builder[T, S]) {

    def labelErrors(label: String): Widget.Builder[T, S] =
      widget.mapErrors(t => new Throwable(s"$label${t.getMessage}", t))

  }

  implicit class WidgetBuilderOpsA[T, S <: { def label: String }](widget: Widget.Builder[T, S]) {

    def autoLabelErrors: Widget.Builder[T, S] =
      widget.mapErrorsWithState((t, s) => new Throwable(s"${s.label}${t.getMessage}", t))

  }

}
object Implicits extends Implicits
