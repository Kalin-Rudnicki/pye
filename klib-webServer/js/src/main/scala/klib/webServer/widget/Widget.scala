package klib.webServer.widget

import scala.annotation.tailrec

import monocle.Lens
import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._
import klib.webServer._

final case class Widget[V, S, A](
    private[webServer] val elementF: (RaiseHandler[S, A], S) => Widget.ElementT,
    private[webServer] val valueF: S => ?[V],
) {

  def render(handleAction: A => Unit)(initialState: S): Widget.ElementT = {
    var currentState: S = initialState

    val raiseHandler: RaiseHandler[S, A] =
      RaiseHandler[S, A] { raises =>
        @tailrec
        def loop(
            queue: List[Raise[S, A]],
        ): Unit =
          queue match {
            case head :: tail =>
              head match {
                case standard: Raise.Standard[S, A] =>
                  standard match {
                    case Raise.UpdateState(updateState, reRender) =>
                      currentState = updateState(currentState)
                      if (reRender) {
                        // TODO (KR) :
                        ???
                      }
                    case Raise.DisplayMessage(message, modifiers, timeout, causeId) =>
                      def getElement(id: String): Maybe[Element] = Maybe(document.getElementById(id))
                      def globalMessages: Maybe[Element] = getElement(Page.Standard.names.PageMessages)

                      causeId.cata(causeId => getElement(s"$causeId-messages"), globalMessages) match {
                        case Some(messagesElement) =>
                          val messageElement =
                            div(
                              message,
                              modifiers,
                            ).render

                          timeout.foreach {
                            window.setTimeout(
                              () => {
                                messagesElement.removeChild(messageElement)
                              },
                              _,
                            )
                          }
                        case None =>
                          // TODO (KR) :
                          window.alert(message)
                      }
                    case history: Raise.History =>
                      history match {
                        case Raise.History.Push(page)    => page.push()
                        case Raise.History.Replace(page) => page.replace()
                        case Raise.History.Go(delta)     => window.history.go(delta)
                      }
                  }
                case Raise.Action(action) =>
                  handleAction(action)
                  loop(tail)
              }
            case Nil =>
          }

        loop(raises)
      }

    elementF(raiseHandler, currentState)
  }

  // =====|  |=====

  def handleAction[A2](f: (S, ?[V], A) => WrappedFuture[List[Raise[S, A2]]]): Widget[V, S, A2] =
    Widget[V, S, A2](
      elementF = { (rh, s) =>
        // TODO (KR) : Test
        val rh2: RaiseHandler[S, A2] = ???
        ???
      },
      valueF = valueF,
    )

  // =====|  |=====

  def zoomOut[S2](lens: Lens[S2, S]): Widget[V, S2, A] =
    Widget[V, S2, A](
      elementF = { (rh: RaiseHandler[S2, A], s: S2) =>
        elementF(
          rh.zoomIn(lens),
          lens.get(s),
        )
      },
      valueF = { (s: S2) =>
        valueF(lens.get(s))
      },
    )

  def wrapElement(combineIn: ConcreteHtmlTag[_ <: Widget.ElemT])(wrapF: Widget.ElemT => Widget.ElemT): Widget[V, S, A] =
    Widget[V, S, A](
      elementF = { (a, s) =>
        val elems = elementF(a, s)
        val elem = if (elems.size == 1) elems.head else combineIn(elems.toList).render

        NonEmptyList(wrapF(elem), Nil)
      },
      valueF = valueF,
    )

  def wrapElements(wrapF: Widget.ElementT => Widget.ElemT): Widget[V, S, A] =
    Widget[V, S, A](
      elementF = (a, s) => NonEmptyList(wrapF(elementF(a, s)), Nil),
      valueF = valueF,
    )

  def labeled(_label: String, combineIn: ConcreteHtmlTag[_ <: Widget.ElemT] = span): Widget[V, S, A] =
    wrapElements { elems =>
      combineIn(
        NonEmptyList
          .nel[Widget.ElementT](
            NonEmptyList.nel(label(_label).render),
            elems,
          )
          .flatten
          .toList,
      ).render
    }

  // =====|  |=====

  def mapValue[V2](f: V => V2): Widget[V2, S, A] =
    Widget[V2, S, A](
      elementF = elementF,
      valueF = valueF(_).map(f),
    )

  def applyValue[V2](f: ?[V => V2]): Widget[V2, S, A] =
    Widget[V2, S, A](
      elementF = elementF,
      valueF = valueF(_).apply(f),
    )

  def flatMapValue[V2](f: V => ?[V2]): Widget[V2, S, A] =
    Widget[V2, S, A](
      elementF = elementF,
      valueF = valueF(_).flatMap(f),
    )

}

object Widget {

  type Projection[S, A] = { type P[V] = Widget[V, S, A] }

  type ElemT = Element
  type ElementT = NonEmptyList[ElemT]

  type SubmitOr[V, S, A] = Widget[V, S, CommonRaise.SubmitOr[A]]
  type Submit[V, S] = Widget[V, S, CommonRaise.Submit.type]
  type NoAction[V, S] = Widget[V, S, Nothing]

  // =====|  |=====

  def builder: Builder1 = new Builder1

  final class Builder1 private[Widget] {

    def withState[S]: Builder2S[S] = new Builder2S[S]

    def noState: Builder2 = new Builder2

    def element[S, A](elem: Widget.ElemT): Widget[Unit, S, A] =
      withState[S].withAction[A].element(elem).noValue

  }

  final class Builder2 private[Widget] {

    def withAction[A]: Builder3A[A] = new Builder3A[A]
    def submitAction: Builder3A[CommonRaise.Submit.type] = withAction[CommonRaise.Submit.type]
    def submitOrAction[O]: Builder3A[CommonRaise.SubmitOr[O]] = withAction[CommonRaise.SubmitOr[O]]

    def noAction: Builder3 = new Builder3

  }
  final class Builder2S[S] private[Widget] {

    def withAction[A]: Builder3SA[S, A] = new Builder3SA[S, A]
    def submitAction: Builder3SA[S, CommonRaise.Submit.type] = withAction[CommonRaise.Submit.type]
    def submitOrAction[O]: Builder3SA[S, CommonRaise.SubmitOr[O]] = withAction[CommonRaise.SubmitOr[O]]

    def noAction: Builder3S[S] = new Builder3S[S]

  }

  final class Builder3 private[Widget] {
    type S = Any
    type A = Nothing
    def element(elem: Widget.ElemT): Builder4[S, A] = new Builder4[S, A]((_, _) => elem)
  }
  final class Builder3S[S] private[Widget] {
    type A = Nothing
    def elementS(elementF: S => Widget.ElemT): Builder4[S, A] = new Builder4[S, A]((_, s) => elementF(s))
    def element(elem: Widget.ElemT): Builder4[S, A] = elementS(_ => elem)
  }
  final class Builder3A[A] private[Widget] {
    type S = Any
    def elementA(elementF: RaiseHandler[S, A] => Widget.ElemT): Builder4[S, A] = new Builder4[S, A]((a, _) => elementF(a))
    def element(elem: Widget.ElemT): Builder4[S, A] = elementA(_ => elem)
  }
  final class Builder3SA[S, A] private[Widget] {
    def elementSA(elementF: (RaiseHandler[S, A], S) => Widget.ElemT): Builder4[S, A] =
      new Builder4[S, A]((a, s) => elementF(a, s))
    def elementS(elementF: S => Widget.ElemT): Builder4[S, A] = elementSA((_, s) => elementF(s))
    def elementA(elementF: RaiseHandler[S, A] => Widget.ElemT): Builder4[S, A] = elementSA((a, _) => elementF(a))
    def element(elem: Widget.ElemT): Builder4[S, A] = elementSA((_, _) => elem)
  }

  final class Builder4[S, A] private[Widget] (elementF: (RaiseHandler[S, A], S) => Widget.ElemT) {

    def withValue[V](valueF: S => ?[V]): Widget[V, S, A] =
      Widget[V, S, A](
        elementF = (a, s) => NonEmptyList(elementF(a, s), Nil),
        valueF = valueF,
      )

    def noValue: Widget[Unit, S, A] = withValue[Unit](_ => ().pure[?])

  }

  // =====|  |=====

  implicit def widgetMonad[S, A]: Monad[Projection[S, A]#P] =
    new Monad[Projection[S, A]#P] {

      override def map[V, V2](t: Widget[V, S, A], f: V => V2): Widget[V2, S, A] =
        t.mapValue(f)

      override def apply[V, V2](t: Widget[V, S, A], f: Widget[V => V2, S, A]): Widget[V2, S, A] =
        Widget[V2, S, A](
          elementF = (rh, s) => NonEmptyList.nel(t.elementF(rh, s), f.elementF(rh, s)).flatten,
          valueF = s => t.valueF(s).apply(f.valueF.apply(s)),
        )

      override def pure[V](a: => V): Widget[V, S, A] =
        Widget.builder.withState[S].withAction[A].element(span.render).withValue(_ => a.pure[?])

      override def flatMap[V, V2](t: Widget[V, S, A], f: V => Widget[V2, S, A]): Widget[V2, S, A] =
        Widget[V2, S, A](
          elementF = { (rh, s) =>
            val tElements = t.elementF(rh, s)

            t.valueF(s) match {
              case Alive(r) =>
                // TODO (KR) : Make sure when `t` updates `S`, `f` re-renders
                NonEmptyList.nel(tElements, f(r).elementF(rh, s)).flatten
              case Dead(_) =>
                tElements
            }
          },
          valueF = s => t.valueF(s).flatMap(f(_).valueF(s)),
        )

    }

}
