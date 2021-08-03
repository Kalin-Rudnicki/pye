package klib.webServer.widget

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

    // TODO (KR) :
    ???
  }

  // =====|  |=====

  def handleAction[A2](f: (S, ?[V], A) => WrappedFuture[List[Raise[S, A2]]]): Widget[V, S, A2] = {

    // TODO (KR) :
    ???
  }

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

  type StdForm[V, S] = Widget[V, S, CommonRaise.Submit.type]
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

      override def map[V, V2](t: Widget[V, S, A], f: V => V2): Widget[V2, S, A] = {
        // TODO (KR) :
        ???
      }

      override def apply[V, V2](t: Widget[V, S, A], f: Widget[V => V2, S, A]): Widget[V2, S, A] = {
        // TODO (KR) :
        ???
      }

      override def pure[V](a: => V): Widget[V, S, A] = {
        // TODO (KR) :
        ???
      }

      override def flatten[V](t: Widget[Widget[V, S, A], S, A]): Widget[V, S, A] = {
        // TODO (KR) :
        ???
      }

    }

}
