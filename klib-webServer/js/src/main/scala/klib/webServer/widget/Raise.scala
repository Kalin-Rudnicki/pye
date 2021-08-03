package klib.webServer.widget

import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import klib.webServer._

sealed trait Raise[+S, +A]
object Raise {
  sealed trait Standard[+S] extends Raise[S, Nothing]
  final case class UpdateState[S](
      updateState: S => S,
      reRender: Boolean = true,
  ) extends Standard[S]
  final case class DisplayMessage(
      message: String,
      modifiers: Seq[Modifier],
      timeout: Maybe[Int],
      causeId: Maybe[String],
  ) extends Standard[Nothing]
  object DisplayMessage {
    final class Builder private[DisplayMessage] (causeId: Maybe[String]) {

      def apply(
          message: String,
          timeout: Maybe[Int] = None,
          modifiers: List[Modifier] = Nil,
      ): DisplayMessage =
        DisplayMessage(
          message = message,
          timeout = timeout,
          modifiers = modifiers,
          causeId = causeId,
        )

      def withClasses(
          message: String,
          classes: List[String],
          timeout: Maybe[Int] = None,
          modifiers: List[Modifier] = Nil,
      ): DisplayMessage =
        apply(
          message = message,
          timeout = timeout,
          modifiers = (`class` := classes.mkString(" ")) :: modifiers,
        )

      def info(
          message: String,
          timeout: Maybe[Int] = None,
          modifiers: List[Modifier] = Nil,
      ): DisplayMessage =
        withClasses(
          message = message,
          classes = List("message-info"),
          timeout = timeout,
          modifiers = modifiers,
        )

      def error(
          message: String,
          timeout: Maybe[Int] = None,
          modifiers: List[Modifier] = Nil,
      ): DisplayMessage =
        withClasses(
          message = message,
          classes = List("message-error"),
          timeout = timeout,
          modifiers = modifiers,
        )

    }

    val global: Builder = new Builder(None)
    def forId(id: String): Builder = new Builder(id.some)
  }
  sealed trait History extends Standard[Nothing]
  object History {
    final case class Push(page: Page[_]) extends History
    final case class Replace(page: Page[_]) extends History
    final case class Go(delta: Int) extends History

    val Forward: Go = Go(1)
    val Pop: Go = Go(-1)
  }

  final case class Action[+A](action: A) extends Raise[Nothing, A]
}
