package pye

import java.util.UUID

import monocle.Lens
import org.scalajs.dom._
import scalatags.JsDom
import scalatags.JsDom
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._
import klib.fp.utils._
import klib.utils._
import pye.Implicits._
import pye.Widget.ElementT
import pye.widgets.modifiers.PyeS

trait Widget[V, S, +A] { thisWidget =>

  def render(
      handleActions: A => AsyncIO[List[Raise.StandardOrUpdate[S]]],
  )(
      initialState: S,
  ): Modifier = {
    val stateVar: Var[S] = Var(initialState)

    val appliedWidget: AppliedWidget[V] =
      Pointer
        .withSelf[AppliedWidget[V]] { ptr =>
          val rh: RaiseHandler[S, A] = { raise =>
            def handleStandardOrUpdate(sou: Raise.StandardOrUpdate[S]): AsyncIO[Unit] =
              sou match {
                case standard: Raise.Standard =>
                  standard match {
                    case msg: Raise.DisplayMessage =>
                      AsyncIO { displayMessage2(msg) }
                    case history: Raise.History =>
                      history match {
                        case Raise.History.Push(page)    => page._push()
                        case Raise.History.Replace(page) => page._replace()
                        case Raise.History.Go(delta)     => AsyncIO { window.history.go(delta) }
                      }
                  }
                case Raise.UpdateState(update, reRender) =>
                  for {
                    _ <- AsyncIO.wrapIO(stateVar.value = update(stateVar.value))
                    _ <- AsyncIO.wrapIO { reRender.maybe(ptr.value.reRender).traverse }
                  } yield ()
              }

            raise match {
              case sou: Raise.StandardOrUpdate[S] =>
                handleStandardOrUpdate(sou)
              case Raise.Action(action) =>
                for {
                  sous <- handleActions(action)
                  _ <- AsyncIO.runSequentially(sous.map(handleStandardOrUpdate))
                } yield ()
            }
          }

          Pointer(thisWidget.convert(rh, stateVar))
        }
        .value

    appliedWidget.reRender.runSyncOrThrow(None).toList
  }

  // =====|  |=====

  // TODO (KR) : Possibly make more efficient
  def mapValue_?[V2](
      mapF: ?[V] => ?[V2],
  ): Widget[V2, S, A] =
    new Widget.MapV[V2, S, A] {
      override final type V1 = V
      override final val w: Widget[V1, S, A] = thisWidget
      override final val f: ?[V1] => ?[V2] = mapF
    }

  // TODO (KR) : Possibly make more efficient
  def mapAction[A0 >: A, A2](
      mapF: (S, ?[V], A0) => AsyncIO[List[Raise[S, A2]]],
  ): Widget[V, S, A2] =
    new Widget.MapA[V, S, A2] {
      override final type A1 = A0
      override final val w: Widget[V, S, A1] = thisWidget
      override final val f: (S, ?[V], A0) => AsyncIO[List[Raise[S, A2]]] = mapF
    }

  // TODO (KR) : Possibly make more efficient
  private def _apply[A0 >: A, V2](
      w: Widget[V => V2, S, A0],
  ): Widget[V2, S, A0] =
    new Widget.Apply[V2, S, A0] {
      override final type V1 = V
      override final val w1: Widget[V1, S, A0] = thisWidget
      override final val w2: Widget[V1 => V2, S, A0] = w
    }

  // TODO (KR) : Possibly make more efficient
  private def _flatMap[A0 >: A, V2](
      wF: V => Widget[V2, S, A0],
  ): Widget[V2, S, A0] =
    new Widget.FlatMap[V2, S, A0] {
      override final type V1 = V
      override final val w1: Widget[V1, S, A0] = thisWidget
      override final val w2: V => Widget[V2, S, A0] = wF
    }

  def wrapped(withElements: Modifier => Widget.ElementT): Widget[V, S, A] =
    new Widget.Wrapped[V, S, A] {
      override private[pye] final val w: Widget[V, S, A] = this
      override private[pye] final val f: JsDom.all.Modifier => ElementT = withElements
    }

  // =====|  |=====

  def zoomOut[S2](s2Lens: Lens[S2, S]): Widget[V, S2, A]

  protected def convert(parentRaiseHandler: RaiseHandler[S, A], stateVar: Var[S]): AppliedWidget[V]

  // =====|  |=====

  def mapValue[V2](
      mapF: V => V2,
  ): Widget[V2, S, A] =
    mapValue_?(_.map(mapF))

  def flatMapValue[V2](
      mapF: V => ?[V2],
  ): Widget[V2, S, A] =
    mapValue_?(_.flatMap(mapF))

  def labelled(
      labelText: String,
      decorators: Seq[Modifier] = Seq.empty,
  ): Widget[V, S, A] =
    ado[Widget.Projection[S, A]#P]
      .join(
        Widget.builder.element[S, A] {
          label(PyeS.`pye:label`)(labelText)(decorators).render
        },
        this,
      )
      .mapValue_? {
        case Alive(r) =>
          r._2.pure[?]
        case Dead(errors) =>
          Dead(errors.map(_.mappedMessage(msg => s"$labelText $msg")))
      }

}

// =====|  |=====

object Widget {

  type ElementT = NonEmptyList[Element]
  type ElemT = Element

  type Submit[V, S] = Widget[V, S, CommonRaise.Submit.type]
  type SubmitOr[V, S, O] = Widget[V, S, CommonRaise.SubmitOr[O]]
  type NoAction[V, S] = Widget[V, S, Nothing]

  // =====|  |=====

  def builder: Builder1 = new Builder1

  final class Builder1 private[Widget] {

    def withState[S]: Builder2[S] = new Builder2[S]

    def elements[S, A](elementF: => Widget.ElementT): Widget[Unit, S, A] =
      withState[S].withAction[A].elements(elementF).noValue
    def element[S, A](elementF: => Widget.ElemT): Widget[Unit, S, A] =
      withState[S].withAction[A].element(elementF).noValue

  }

  final class Builder2[S] private[Widget] {

    def withAction[A]: Builder3[S, A] = new Builder3[S, A]
    def submitAction: Builder3[S, CommonRaise.Submit.type] = withAction[CommonRaise.Submit.type]
    def submitOrAction[O]: Builder3[S, CommonRaise.SubmitOr[O]] = withAction[CommonRaise.SubmitOr[O]]

    def noAction: Builder3[S, Nothing] = withAction[Nothing]

  }

  final class Builder3[S, A] private[Widget] {

    def rsElements(elementF: RaiseHandler[S, A] => S => Widget.ElementT): Builder4[S, A] =
      new Builder4[S, A](elementF)
    def rElements(elementF: RaiseHandler[S, A] => Widget.ElementT): Builder4[S, A] =
      rsElements(r => _ => elementF(r))
    def sElements(elementF: S => Widget.ElementT): Builder4[S, A] =
      rsElements(_ => elementF)
    def elements(elementF: => Widget.ElementT): Builder4[S, A] =
      rsElements(_ => _ => elementF)

    def rsElement(elementF: RaiseHandler[S, A] => S => Widget.ElemT): Builder4[S, A] =
      rsElements(r => s => NonEmptyList(elementF(r)(s), Nil))
    def rElement(elementF: RaiseHandler[S, A] => Widget.ElemT): Builder4[S, A] =
      rsElement(r => _ => elementF(r))
    def sElement(elementF: S => Widget.ElemT): Builder4[S, A] =
      rsElement(_ => elementF)
    def element(elementF: => Widget.ElemT): Builder4[S, A] =
      rsElement(_ => _ => elementF)

  }

  final class Builder4[S, A] private[Widget] (elementF: RaiseHandler[S, A] => S => Widget.ElementT) {

    def withValue[V](valueF: S => ?[V]): Widget[V, S, A] = {
      val _elementF = elementF
      val _valueF = valueF

      new Widget.Leaf[V, S, A] {
        override private[pye] final type LeafS = S
        override private[pye] final type LeafA = A
        override private[pye] final val lens: Lens[S, S] = Lens.id[S]
        override private[pye] final val elementF: RaiseHandler[S, A] => S => ElementT = _elementF
        override private[pye] final val valueF: S => ?[V] = _valueF
      }
    }

    def noValue: Widget[Unit, S, A] = withValue[Unit](_ => ().pure[?])

  }

  // =====|  |=====

  def rhCaptureReRender[V, S, A](
      w: Pointer[AppliedWidget[V]],
      parentRH: RaiseHandler[S, A],
      afterUpdate: IO[Unit] = IO {},
  ): RaiseHandler[S, A] = {
    case sou: Raise.StandardOrUpdate[S] =>
      sou match {
        case standard: Raise.Standard =>
          parentRH._handleRaise(standard)
        case update: Raise.UpdateState[S] =>
          for {
            _ <- parentRH._handleRaise(Raise.UpdateState[S](update.update, false))
            _ <- update.reRender ? AsyncIO.wrapIO(w.value.reRender).map { _ => } | AsyncIO {}
            _ <- AsyncIO.wrapIO(afterUpdate)
          } yield ()
      }
    case action: Raise.Action[A] =>
      parentRH._handleRaise(action)
  }

  private[pye] def replaceNodes(oldElems: Widget.ElementT, newElems: Widget.ElementT): IO[Unit] = {
    val parent = oldElems.head.parentNode
    val addNode: Node => Unit =
      Maybe(oldElems.toList.last.nextSibling) match {
        case Some(nextSibling) =>
          parent.insertBefore(_, nextSibling)
        case None =>
          parent.appendChild
      }

    IO {
      oldElems.foreach(parent.removeChild)
      newElems.foreach(addNode)
    }
  }

  // =====|  |=====

  sealed trait Leaf[V, S, +A] extends Widget[V, S, A] { thisWidget =>
    private[pye] type LeafS
    private[pye] type LeafA
    private[pye] val lens: Lens[S, LeafS]
    private[pye] val elementF: RaiseHandler[LeafS, LeafA] => LeafS => Widget.ElementT
    private[pye] val valueF: LeafS => ?[V]

    override final def zoomOut[S2](s2Lens: Lens[S2, S]): Widget[V, S2, A] =
      new Widget.Leaf[V, S2, A] {
        override private[pye] final type LeafS = thisWidget.LeafS
        override private[pye] final type LeafA = thisWidget.LeafA
        override private[pye] final val lens: Lens[S2, LeafS] = s2Lens.andThen(thisWidget.lens)
        override private[pye] final val elementF: RaiseHandler[LeafS, LeafA] => LeafS => Widget.ElementT =
          thisWidget.elementF
        override private[pye] final val valueF: LeafS => ?[V] = thisWidget.valueF
      }

    override final def convert(parentRaiseHandler: RaiseHandler[S, A], stateVar: Var[S]): AppliedWidget[V] = {
      // NOTE : Trusting the types that by the time the RaiseHandler gets here, it has the correct `A` type
      val pRH: RaiseHandler[S, thisWidget.LeafA] =
        parentRaiseHandler.asInstanceOf[RaiseHandler[S, thisWidget.LeafA]]

      val leafRH: RaiseHandler[thisWidget.LeafS, thisWidget.LeafA] = {
        case sou: Raise.StandardOrUpdate[thisWidget.LeafS] =>
          sou match {
            case standard: Raise.Standard =>
              pRH._handleRaise(standard)
            case update: Raise.UpdateState[thisWidget.LeafS] =>
              pRH._handleRaise(Raise.UpdateState[S](thisWidget.lens.modify(update.update), update.reRender))
          }
        case action: Raise.Action[thisWidget.LeafA] =>
          pRH._handleRaise(action)
      }

      new AppliedWidget[V] {
        private val elems: Var[Maybe[Widget.ElementT]] = Var(None)

        override private[pye] final val value: IO[V] =
          IO.wrapEffect { thisWidget.valueF(thisWidget.lens.get(stateVar.value)) }
        override private[pye] final val current: IO[Maybe[Widget.ElementT]] =
          IO { elems.value }
        override private[pye] final val getElementsAndUpdate: IO[Widget.ElementT] =
          for {
            newElems <- IO { thisWidget.elementF(leafRH)(thisWidget.lens.get(stateVar.value)) }
            _ <- elems.value = newElems.some
          } yield newElems
      }
    }

  }

  sealed trait MapV[V, S, +A] extends Widget[V, S, A] { thisWidget =>
    private[pye] type V1
    private[pye] val w: Widget[V1, S, A]
    private[pye] val f: ?[V1] => ?[V]

    override final def zoomOut[S2](s2Lens: Lens[S2, S]): Widget[V, S2, A] =
      new Widget.MapV[V, S2, A] {
        override private[pye] final type V1 = thisWidget.V1
        override private[pye] final val w: Widget[V1, S2, A] = thisWidget.w.zoomOut(s2Lens)
        override private[pye] final val f: ?[V1] => ?[V] = thisWidget.f
      }

    override final def convert(parentRaiseHandler: RaiseHandler[S, A], stateVar: Var[S]): AppliedWidget[V] = {
      val child: AppliedWidget[thisWidget.V1] = thisWidget.w.convert(parentRaiseHandler, stateVar)

      new AppliedWidget[V] {
        override private[pye] final val value: IO[V] =
          IO.wrapEffect { thisWidget.f(child.value.runSync) }
        override private[pye] final val current: IO[Maybe[Widget.ElementT]] =
          child.current
        override private[pye] final val getElementsAndUpdate: IO[Widget.ElementT] =
          child.getElementsAndUpdate
      }
    }

  }

  sealed trait MapA[V, S, +A] extends Widget[V, S, A] { thisWidget =>
    private[pye] type A1
    private[pye] val w: Widget[V, S, A1]
    private[pye] val f: (S, ?[V], A1) => AsyncIO[List[Raise[S, A]]]

    override final def zoomOut[S2](s2Lens: Lens[S2, S]): Widget[V, S2, A] =
      new Widget.MapA[V, S2, A] {
        override private[pye] final type A1 = thisWidget.A1
        override private[pye] final val w: Widget[V, S2, A1] = thisWidget.w.zoomOut(s2Lens)
        override private[pye] final val f: (S2, ?[V], A1) => AsyncIO[List[Raise[S2, A]]] = { (s2, v, a) =>
          thisWidget.f(s2Lens.get(s2), v, a).map {
            _.map {
              case action: Raise.Action[A] => action
              case sou: Raise.StandardOrUpdate[S] =>
                sou match {
                  case standard: Raise.Standard     => standard
                  case update: Raise.UpdateState[S] => Raise.UpdateState(s2Lens.modify(update.update), update.reRender)
                }
            }
          }
        }
      }

    override final def convert(parentRaiseHandler: RaiseHandler[S, A], stateVar: Var[S]): AppliedWidget[V] =
      Pointer
        .withSelf[AppliedWidget[V]] { ptr =>
          val mappedRH: RaiseHandler[S, thisWidget.A1] = {
            case sou: Raise.StandardOrUpdate[S] =>
              parentRaiseHandler._handleRaise(sou)
            case action: Raise.Action[thisWidget.A1] =>
              for {
                raises <- thisWidget.f(stateVar.value, ptr.value.value.runSync, action.action)
                _ <- parentRaiseHandler._handleRaises(raises)
              } yield ()
          }

          Pointer(thisWidget.w.convert(mappedRH, stateVar))
        }
        .value

  }

  sealed trait Apply[V, S, +A] extends Widget[V, S, A] { thisWidget =>
    private[pye] type V1
    private[pye] val w1: Widget[V1, S, A]
    private[pye] val w2: Widget[V1 => V, S, A]

    override final def zoomOut[S2](s2Lens: Lens[S2, S]): Widget[V, S2, A] =
      new Widget.Apply[V, S2, A] {
        override private[pye] final type V1 = thisWidget.V1
        override private[pye] final val w1: Widget[V1, S2, A] = thisWidget.w1.zoomOut(s2Lens)
        override private[pye] final val w2: Widget[V1 => V, S2, A] = thisWidget.w2.zoomOut(s2Lens)
      }

    override final def convert(parentRaiseHandler: RaiseHandler[S, A], stateVar: Var[S]): AppliedWidget[V] = {
      val w1: AppliedWidget[thisWidget.V1] =
        Pointer
          .withSelf[AppliedWidget[thisWidget.V1]] { ptr =>
            val rh: RaiseHandler[S, A] = Widget.rhCaptureReRender(ptr, parentRaiseHandler)
            Pointer(thisWidget.w1.convert(rh, stateVar))
          }
          .value
      val w2: AppliedWidget[thisWidget.V1 => V] =
        Pointer
          .withSelf[AppliedWidget[thisWidget.V1 => V]] { ptr =>
            val rh: RaiseHandler[S, A] = Widget.rhCaptureReRender(ptr, parentRaiseHandler)
            Pointer(thisWidget.w2.convert(rh, stateVar))
          }
          .value

      new AppliedWidget[V] {
        override private[pye] final val value: IO[V] =
          w1.value.apply(w2.value)
        override private[pye] final val current: IO[Maybe[Widget.ElementT]] =
          ado[MaybeMonad.Projection[IO]#P]
            .join(
              w1.current.toMaybeMonad,
              w2.current.toMaybeMonad,
            )
            .map {
              case (w1E, w2E) =>
                NonEmptyList.nel(w1E, w2E).flatten
            }
            .wrapped
        override private[pye] final val getElementsAndUpdate: IO[Widget.ElementT] =
          ado[IO]
            .join(
              w1.getElementsAndUpdate,
              w2.getElementsAndUpdate,
            )
            .map {
              case (w1E, w2E) =>
                NonEmptyList.nel(w1E, w2E).flatten
            }
      }
    }

  }

  sealed trait FlatMap[V, S, +A] extends Widget[V, S, A] { thisWidget =>
    private[pye] type V1
    private[pye] val w1: Widget[V1, S, A]
    private[pye] val w2: V1 => Widget[V, S, A]

    override final def zoomOut[S2](s2Lens: Lens[S2, S]): Widget[V, S2, A] =
      new Widget.FlatMap[V, S2, A] {
        override private[pye] final type V1 = thisWidget.V1
        override private[pye] final val w1: Widget[V1, S2, A] = thisWidget.w1.zoomOut(s2Lens)
        override private[pye] final val w2: V1 => Widget[V, S2, A] = thisWidget.w2(_).zoomOut(s2Lens)
      }

    override final def convert(parentRaiseHandler: RaiseHandler[S, A], stateVar: Var[S]): AppliedWidget[V] = {
      def makeW2(v1: thisWidget.V1): AppliedWidget[V] =
        Pointer
          .withSelf[AppliedWidget[V]] { ptr =>
            val rh: RaiseHandler[S, A] = Widget.rhCaptureReRender(ptr, parentRaiseHandler)
            Pointer(thisWidget.w2(v1).convert(rh, stateVar))
          }
          .value

      lazy val w1: AppliedWidget[thisWidget.V1] =
        Pointer
          .withSelf[AppliedWidget[thisWidget.V1]] { ptr =>
            val rh: RaiseHandler[S, A] =
              Widget.rhCaptureReRender(
                ptr,
                parentRaiseHandler,
                w2.reRender.map { _ => },
              )
            Pointer(thisWidget.w1.convert(rh, stateVar))
          }
          .value

      lazy val w2: AppliedWidget[V] =
        new AppliedWidget[V] {
          private val inner: Var[Maybe[Widget.ElementT] \/ AppliedWidget[V]] = Var(None.left)

          override private[pye] final val value: IO[V] = {
            for {
              // TODO (KR) : There is a bug here...
              //           : Somehow adding this blank `IO {}` fixes it
              //           : (there are 2 of these)
              _ <- IO {}

              v1 <- w1.value
              _w2 <- IO { makeW2(v1) }
              v2 <- _w2.value
            } yield v2
          }
          override private[pye] final val current: IO[Maybe[Widget.ElementT]] = {
            for {
              evalInner <- IO { inner.value }
              elems <- evalInner match {
                case Right(w) => w.current
                case Left(e)  => IO { e }
              }
            } yield elems
          }
          override private[pye] final val getElementsAndUpdate: IO[Widget.ElementT] = {
            for {
              v1_? <- IO { w1.value.runSync }
              elems <- v1_? match {
                case Alive(r) =>
                  for {
                    w2 <- IO { makeW2(r) }
                    newElems <- w2.getElementsAndUpdate
                    _ <- inner.value = w2.right
                  } yield newElems
                case Dead(_) =>
                  for {
                    newElem <- IO { span(id := UUID.randomUUID.toString).render }
                    newElems = NonEmptyList.nel(newElem)
                    _ <- inner.value = newElems.some.left
                  } yield newElems
              }
            } yield elems
          }
        }

      /*
        NOTE : Current approach is not going to work
             : There is no way to have the second part of flatMap know how to reRender
       */

      new AppliedWidget[V] {
        override private[pye] final val current: IO[Maybe[Widget.ElementT]] =
          ado[MaybeMonad.Projection[IO]#P]
            .join(
              w1.current.toMaybeMonad,
              w2.current.toMaybeMonad,
            )
            .map {
              case (w1E, w2E) =>
                NonEmptyList.nel(w1E, w2E).flatten
            }
            .wrapped
        override private[pye] final val value: IO[V] =
          w2.value
        override private[pye] final val getElementsAndUpdate: IO[Widget.ElementT] =
          ado[IO]
            .join(
              w1.getElementsAndUpdate,
              w2.getElementsAndUpdate,
            )
            .map {
              case (w1E, w2E) =>
                NonEmptyList.nel(w1E, w2E).flatten
            }
      }
    }

  }

  sealed trait Wrapped[V, S, +A] extends Widget[V, S, A] { thisWidget =>
    private[pye] val w: Widget[V, S, A]
    private[pye] val f: Modifier => Widget.ElementT

    override final def zoomOut[S2](s2Lens: Lens[S2, S]): Widget[V, S2, A] =
      new Widget.Wrapped[V, S2, A] {
        override private[pye] final val w: Widget[V, S2, A] = thisWidget.w.zoomOut(s2Lens)
        override private[pye] final val f: JsDom.all.Modifier => ElementT = thisWidget.f
      }

    override final def convert(parentRaiseHandler: RaiseHandler[S, A], stateVar: Var[S]): AppliedWidget[V] = {
      val child: AppliedWidget[V] =
        Pointer
          .withSelf[AppliedWidget[V]] { ptr =>
            val rh: RaiseHandler[S, A] = Widget.rhCaptureReRender(ptr, parentRaiseHandler)
            Pointer(thisWidget.w.convert(rh, stateVar))
          }
          .value

      new AppliedWidget[V] {
        private val elems: Var[Maybe[Widget.ElementT]] = Var(None)

        override private[pye] final val value: IO[V] =
          child.value
        override private[pye] final val current: IO[Maybe[Widget.ElementT]] =
          IO { elems.value }
        override private[pye] final val getElementsAndUpdate: IO[Widget.ElementT] =
          for {
            childElems <- child.getElementsAndUpdate
            myElems = thisWidget.f(childElems.toList)
            _ <- elems.value = myElems.some
          } yield myElems
      }
    }

  }

  // =====|  |=====

  type Projection[S, A] = { type P[V] = Widget[V, S, A] }

  implicit def widgetMonad[S, A]: Monad[Widget.Projection[S, A]#P] =
    new Monad[Projection[S, A]#P] {

      override def map[V1, V2](t: Widget[V1, S, A], f: V1 => V2): Widget[V2, S, A] =
        t.mapValue(f)

      override def apply[V1, V2](t: Widget[V1, S, A], f: Widget[V1 => V2, S, A]): Widget[V2, S, A] =
        t._apply(f)

      // NOTE : This should really never be used, but is needed for Applicative
      //      : It would also be nice if ElementT could be an empty List,
      //      : but at least for the time being, NonEmptyList is necessary
      override def pure[V1](a: => V1): Widget[V1, S, A] =
        Widget.builder.withState[S].withAction[A].element(span.render).withValue(_ => a.pure[?])

      override def flatMap[V1, V2](t: Widget[V1, S, A], f: V1 => Widget[V2, S, A]): Widget[V2, S, A] =
        t._flatMap(f)

    }

  /*
  implicit def widgetTraverseList[S, A]: Traverse[List, Widget.Projection[S, A]#P] =
    new Traverse[List, Widget.Projection[S, A]#P] {

      override def traverse[T](t: List[Widget[T, S, A]]): Widget[List[T], S, A] = ???

    }
   */

}

sealed trait AppliedWidget[V] {

  private[pye] val current: IO[Maybe[Widget.ElementT]]
  private[pye] val value: IO[V]
  private[pye] val getElementsAndUpdate: IO[Widget.ElementT]

  private[pye] final val reRender: IO[Widget.ElementT] =
    for {
      // TODO (KR) : There is a bug here...
      //           : Somehow adding this blank `IO {}` fixes it
      //           : (there are 2 of these)
      _ <- IO {}

      mOldElements <- current
      newElements <- getElementsAndUpdate
      _ <- mOldElements.map(Widget.replaceNodes(_, newElements)).traverse
    } yield newElements

}
