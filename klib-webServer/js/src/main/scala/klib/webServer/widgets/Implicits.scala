package klib.webServer.widgets

import scalatags.JsDom.all._

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

  implicit class InputStateWidgetBuilderOps[T](widget: Widget.Builder[T, inputs.InputState]) {

    def reduceToVar(
        label: String,
        id: String,
        labelModifiers: Seq[Modifier] = Seq.empty,
        inputModifiers: Seq[Modifier] = Seq.empty,
        errorsModifiers: Seq[Modifier] = Seq.empty,
        containerModifiers: Seq[Modifier] = Seq.empty,
    ): Widget.Builder[T, Var[String]] =
      widget
        .rMapState[Var[String]] { s =>
          inputs.InputState(
            label = label,
            id = id,
            state = s,
            labelModifiers = labelModifiers,
            inputModifiers = inputModifiers,
            errorsModifiers = errorsModifiers,
            containerModifiers = containerModifiers,
          )
        }

  }

}
object Implicits extends Implicits
