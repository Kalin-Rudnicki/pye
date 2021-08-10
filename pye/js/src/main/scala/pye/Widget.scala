package pye

import java.util.UUID

import monocle.Lens
import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._
import klib.utils.Var

final case class Widget[V, S, +A](
    private[pye] val elementF: (RaiseHandler[S, A], S) => Widget.ElementT,
    private[pye] val valueF: S => ?[V],
) {
  type Value = V
  type State = S
  type Action <: A

  def render(handleAction: A => AsyncIO[List[Raise.Standard[S]]])(initialState: S): Widget.ElementT = {
    val elements = Var.`null`[Widget.ElementT]

    val raiseHandler: RaiseHandler[S, A] =
      RaiseHandler
        .globalRaiseHandler[S, A](initialState, handleAction)
        .captureUpdateState(this, elements)()

    raiseHandler._global = true

    (elements.value = elementF(raiseHandler, raiseHandler._state)).runSyncOrDump(None)

    elements.value
  }

  // =====|  |=====

  def handleRaise[A2](
      f: (S, ?[V], Raise[S, A]) => AsyncIO[List[Raise[S, A2]]],
  ): Widget[V, S, A2] =
    Widget[V, S, A2](
      elementF = { (rh, s) =>
        lazy val rh2: RaiseHandler[S, A] =
          new RaiseHandler[S, A](
            initialState = rh.initialState,
            handleRaise = { raise =>
              for {
                _ <- raise match {
                  case updateState: Raise.UpdateState[S] =>
                    AsyncIO { rh2._state = updateState.updateState(rh2._state) }
                  case _ =>
                    ().pure[AsyncIO]
                }
                newRaises <- f(rh2._state, valueF(rh2._state), raise)
                _ <- rh.handleRaises(newRaises)
              } yield ()
            },
          )

        elementF(rh2, s)
      },
      valueF = valueF,
    )

  def handleAction[A2](
      f: (S, ?[V], A) => AsyncIO[List[Raise[S, A2]]],
  ): Widget[V, S, A2] =
    handleRaise { (s, v, raise) =>
      raise match {
        case Raise.Action(action) =>
          f(s, v, action)
        case standard: Raise.Standard[S] =>
          (standard :: Nil).pure[AsyncIO]
      }
    }

  def onUpdateState[A2 >: A](
      f: (S, ?[V], Raise.UpdateState[S]) => AsyncIO[List[Raise[S, A2]]],
  ): Widget[V, S, A2] =
    handleRaise { (s, v, raise) =>
      raise match {
        case updateState: Raise.UpdateState[S] =>
          for {
            newRaises <- f(s, v, updateState)
          } yield newRaises
        case r =>
          (r :: Nil).pure[AsyncIO]
      }
    }

  // =====|  |=====

  def zoomOut[S2](lens: Lens[S2, S]): Widget[V, S2, A] = {
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
  }

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

  def labeled(
      _label: String,
      combineIn: ConcreteHtmlTag[_ <: Widget.ElemT] = span,
      labelDecorators: Seq[Modifier] = Seq.empty,
  ): Widget[V, S, A] =
    wrapElements { elems =>
      combineIn(
        NonEmptyList
          .nel[Widget.ElementT](
            NonEmptyList.nel(label(_label)(labelDecorators).render),
            elems,
          )
          .flatten
          .toList,
      ).render
    }.mapError { throwable =>
      Message(s"${_label} ${Maybe(throwable.getMessage).getOrElse(throwable.toString)}")
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

  def mapError(f: Throwable => Throwable): Widget[V, S, A] =
    Widget[V, S, A](
      elementF = elementF,
      valueF = valueF(_) match {
        case Alive(r) =>
          r.pure[?]
        case Dead(errors) =>
          Dead(errors.map(f))
      },
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

    def element[S, A](elem: => Widget.ElemT): Widget[Unit, S, A] =
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
    def element(elem: => Widget.ElemT): Builder4[S, A] = new Builder4[S, A]((_, _) => elem)
  }
  final class Builder3S[S] private[Widget] {
    type A = Nothing
    def elementS(elementF: S => Widget.ElemT): Builder4[S, A] = new Builder4[S, A]((_, s) => elementF(s))
    def element(elem: => Widget.ElemT): Builder4[S, A] = elementS(_ => elem)
  }
  final class Builder3A[A] private[Widget] {
    type S = Any
    def elementA(elementF: RaiseHandler[S, A] => Widget.ElemT): Builder4[S, A] = new Builder4[S, A]((a, _) => elementF(a))
    def element(elem: => Widget.ElemT): Builder4[S, A] = elementA(_ => elem)
  }
  final class Builder3SA[S, A] private[Widget] {
    def elementSA(elementF: (RaiseHandler[S, A], S) => Widget.ElemT): Builder4[S, A] =
      new Builder4[S, A]((a, s) => elementF(a, s))
    def elementS(elementF: S => Widget.ElemT): Builder4[S, A] = elementSA((_, s) => elementF(s))
    def elementA(elementF: RaiseHandler[S, A] => Widget.ElemT): Builder4[S, A] = elementSA((a, _) => elementF(a))
    def element(elem: => Widget.ElemT): Builder4[S, A] = elementSA((_, _) => elem)
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
          elementF = { (rh, s) =>
            val tE = Var.`null`[Widget.ElementT]
            val fE = Var.`null`[Widget.ElementT]

            val rhT = rh.captureUpdateState(t, tE)()
            val rhF = rh.captureUpdateState(f, fE)()

            {
              for {
                _ <- tE.value = t.elementF(rhT, s)
                _ <- fE.value = f.elementF(rhF, s)
              } yield ()
            }.runSyncOrDump(None)

            NonEmptyList
              .nel(
                tE.value,
                fE.value,
              )
              .flatten
          },
          valueF = s => t.valueF(s).apply(f.valueF.apply(s)),
        )

      // NOTE : This should really never be used,
      //      : but it is required for applicative
      override def pure[V](a: => V): Widget[V, S, A] =
        Widget.builder.withState[S].withAction[A].element(span.render).withValue(_ => a.pure[?])

      override def flatMap[V, V2](t: Widget[V, S, A], f: V => Widget[V2, S, A]): Widget[V2, S, A] =
        Widget[V2, S, A](
          elementF = { (rh, s) =>
            val tE = Var.`null`[Widget.ElementT]
            val fE = Var.`null`[Widget.ElementT]

            def calcFElements(s: S): Widget.ElementT =
              t.valueF(s) match {
                case Alive(r) =>
                  val fW = f(r)
                  val rh2 = rh.captureUpdateState(fW, fE)()
                  fW.elementF(rh2, s)
                case Dead(_) =>
                  NonEmptyList.nel(span(id := UUID.randomUUID.toString).render)
              }

            val rh1 = rh.captureUpdateState(t, tE) { s =>
              val newFElems = calcFElements(s)
              RaiseHandler.replaceNodes(fE.value, newFElems)
              (fE.value = newFElems).runSyncOrDump(None)
            }

            {
              for {
                _ <- tE.value = t.elementF(rh1, s)
                _ <- fE.value = calcFElements(rh._state)
              } yield ()
            }.runSyncOrDump(None)

            val tmp =
              NonEmptyList
                .nel(
                  tE.value,
                  fE.value,
                )
                .flatten

            tmp
          },
          valueF = s => t.valueF(s).flatMap(f(_).valueF(s)),
        )

    }

  // =====|  |=====

  def required[V, S, A](widget: Widget[Maybe[V], S, A]): Widget[V, S, A] =
    widget.flatMapValue(_.toEA(Message("Missing required value")))

}
