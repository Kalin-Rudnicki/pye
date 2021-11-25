package pye

import org.scalajs.dom.Element
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

    def toFormMapO[A](
        endpoint: V => AsyncIO[List[Raise[S, A]]],
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
              } yield endpointRes
            case SubmitOr.Or(or) =>
              mapO(or)
          }
        }

    def toForm[A >: O](
        endpoint: V => AsyncIO[List[Raise[S, A]]],
        submitButtonLabel: String = "Submit",
    ): Widget[V, S, A] =
      toFormMapO(
        endpoint = endpoint,
        submitButtonLabel = submitButtonLabel,
        mapO = o => AsyncIO(Raise.Action(o) :: Nil),
      )

  }

  implicit class KeyedActionWidgetOps[V, S, KA_S, KA_K, KA_A](
      widget: Widget[V, S, widgets.all.KeyedAction[(KA_S, KA_K), KA_A]],
  ) {

    def stripKeyedAction: Widget[V, S, KA_A] =
      widget.mapAction[widgets.all.KeyedAction[(KA_S, KA_K), KA_A], KA_A] { (_, _, a) =>
        AsyncIO { Raise.Action(a.action) :: Nil }
      }

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

  implicit class HtmlTagOps(tag: ConcreteHtmlTag[_ <: Element]) {

    def asWidget[Env, A]: Widget[Unit, Env, A] =
      Widget.builder.element(tag.render)

  }

}
object Implicits extends Implicits with CSS.Implicits
