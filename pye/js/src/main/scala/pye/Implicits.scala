package pye

import scala.concurrent.ExecutionContext

import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils._
import pye.CommonRaise.SubmitOr

trait Implicits {

  implicit class MaybeWidgetOps[V, S, A](widget: Widget[Maybe[V], S, A]) {

    def required(): Widget[V, S, A] =
      Widget.required(widget)

  }

  implicit class NoActionWidgetOps[V, S](widget: Widget.NoAction[V, S]) {

    def renderNoAction(initialState: S)(implicit ec: ExecutionContext): Widget.ElementT =
      widget.render { _ => Nil.pure[AsyncIO] }(initialState)

  }

  implicit class WidgetFormOps[V, S, O](widget: Widget[V, S, CommonRaise.SubmitOr[O]]) {

    def toFormMapO[R, A](
        endpoint: V => AsyncIO[R],
        thenRaise: R => AsyncIO[List[Raise[S, A]]],
        mapO: O => AsyncIO[List[Raise[S, A]]],
        submitButtonLabel: String = "Submit",
    )(implicit ec: ExecutionContext): Widget[V, S, A] =
      ado[Widget.Projection[S, CommonRaise.SubmitOr[O]]#P]
        .join(
          widget,
          Widget.builder.element(br.render),
          widgets.forms.submitButton(submitButtonLabel),
        )
        .mapValue(_._1)
        .handleAction { (_, v, a) =>
          a match {
            case CommonRaise.Submit =>
              for {
                aliveV <- AsyncIO.wrapEffect(v)
                endpointRes <- endpoint(aliveV)
                thenRaiseRes <- thenRaise(endpointRes)
              } yield thenRaiseRes
            case SubmitOr.Or(or) =>
              mapO(or)
          }
        }

    def toForm[R, A >: O](
        endpoint: V => AsyncIO[R],
        thenRaise: R => AsyncIO[List[Raise[S, A]]],
        submitButtonLabel: String = "Submit",
    )(implicit ec: ExecutionContext): Widget[V, S, A] =
      toFormMapO(
        endpoint = endpoint,
        thenRaise = thenRaise,
        submitButtonLabel = submitButtonLabel,
        mapO = o => AsyncIO(Raise.Action(o) :: Nil),
      )

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
