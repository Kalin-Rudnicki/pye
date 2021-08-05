package pye

import scala.concurrent.ExecutionContext

import klib.Implicits._
import klib.fp.types._

trait Implicits {

  implicit class MaybeWidgetOps[V, S, A](widget: Widget[Maybe[V], S, A]) {

    def required(): Widget[V, S, A] =
      Widget.required(widget)

  }

  implicit class NoActionWidgetOps[V, S](widget: Widget.NoAction[V, S]) {

    def renderNoAction(initialState: S)(implicit ec: ExecutionContext): Widget.ElementT =
      widget.render { _ => Nil.pure[AsyncIO] }(initialState)

  }

  /*
  implicit class SubmitWidgetOps[V, S](widget: Widget[V, S, CommonRaise.Submit.type]) {

    def toForm[A](
    ): Widget[V, S, A] = {
      // TODO (KR) :
      ???
    }

    def toFormNoRaise(
    ): Widget[V, S, Nothing] = {
      // TODO (KR) :
      ???
    }

  }

  implicit class SubmitOrWidgetOps[V, S, O](widget: Widget[V, S, CommonRaise.SubmitOr[O]]) {

    def toFormOr(
    ): Widget[V, S, O] = {
      // TODO (KR) :
      ???
    }

    def toFormMapOr[A](
        mapOr: O => A,
    ): Widget[V, S, A] = {
      // TODO (KR) :
      ???
    }

  }
   */

  implicit class AsyncIOOps[T](asyncIO: AsyncIO[T]) {

    def runAndShowErrors(onComplete: T => Unit = (_: T) => ())(implicit ec: ExecutionContext): Unit =
      asyncIO.runASync {
        case Alive(res) =>
          onComplete(res)
        case Dead(errors) =>
          errors.map(Raise.DisplayMessage.fromThrowable).foreach(displayMessage)
      }

  }

}
object Implicits extends Implicits with CSS.Implicits
