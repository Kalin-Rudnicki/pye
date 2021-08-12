package pye

import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils._
import pye.CommonRaise.SubmitOr

trait Implicits {

  implicit class MaybeWidgetOps[V, S, A](widget: Widget[Maybe[V], S, A]) {

    def required(message: String = "Missing required value"): Widget[V, S, A] =
      widget.flatMapValue(_.toEA(Message(message)))

  }

  // TODO (KR) : Make builder.
  //           : Scala is having an un-acceptable level type inference here
  implicit class WidgetFormOps[V, S, O](widget: Widget[V, S, CommonRaise.SubmitOr[O]]) {

    def toFormMapO[R, A](
        endpoint: V => AsyncIO[R],
        thenRaise: R => AsyncIO[List[Raise[S, A]]],
        mapO: O => AsyncIO[List[Raise[S, A]]],
        submitButtonLabel: String = "Submit",
    ): Widget[V, S, A] =
      ado[Widget.Projection[S, CommonRaise.SubmitOr[O]]#P]
        .join(
          widget,
          Widget.builder.element(br.render),
          widgets.forms.submitButton(submitButtonLabel),
        )
        .mapValue(_._1)
        .mapAction { (_, v, a: SubmitOr[O]) =>
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
    ): Widget[V, S, A] =
      toFormMapO(
        endpoint = endpoint,
        thenRaise = thenRaise,
        submitButtonLabel = submitButtonLabel,
        mapO = o => AsyncIO(Raise.Action(o) :: Nil),
      )

  }

  implicit class AsyncIOOps[T](asyncIO: AsyncIO[T]) {

    def runAndShowErrors(onComplete: T => Unit = (_: T) => ()): Unit =
      asyncIO.runASyncGlobal {
        case Alive(res) =>
          onComplete(res)
        case Dead(errors) =>
          errors.map(Raise.DisplayMessage.fromThrowable).foreach(displayMessage)
      }

  }

}
object Implicits extends Implicits with CSS.Implicits
