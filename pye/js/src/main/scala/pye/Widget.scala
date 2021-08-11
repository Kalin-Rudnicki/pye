package pye

import java.util.UUID

import monocle.Lens
import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._
import klib.fp.utils._
import klib.utils._
import pye.Implicits._
import pye.widgets.modifiers.PyeS

sealed trait Widget[V, S, +A] { thisWidget =>

  def render(
      handleActions: A => AsyncIO[List[Raise.StandardOrUpdate[S]]],
  )(
      initialState: S,
  ): Modifier = {
    val stateVar: Var[S] = Var(initialState)

    def rhCaptureReRender[_V, A2](
        w: Pointer[AppliedWidget[_V]],
        parentRH: RaiseHandler[S, A2],
        afterUpdate: IO[Unit],
    ): RaiseHandler[S, A2] = {
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

    def convert[V2, A2](
        widget: Widget[V2, S, A2],
        parentRaiseHandler: RaiseHandler[S, A2],
    ): AppliedWidget[V2] = {
      widget match {
        case leaf: Widget.Leaf[V2, S, A2] =>
          // NOTE : Trusting the types that by the time the RaiseHandler gets here, it has the correct `A` type
          val pRH: RaiseHandler[S, leaf.LeafA] =
            parentRaiseHandler.asInstanceOf[RaiseHandler[S, leaf.LeafA]]

          val leafRH: RaiseHandler[leaf.LeafS, leaf.LeafA] = {
            case sou: Raise.StandardOrUpdate[leaf.LeafS] =>
              sou match {
                case standard: Raise.Standard =>
                  pRH._handleRaise(standard)
                case update: Raise.UpdateState[leaf.LeafS] =>
                  pRH._handleRaise(Raise.UpdateState[S](leaf.lens.modify(update.update), update.reRender))
              }
            case action: Raise.Action[leaf.LeafA] =>
              pRH._handleRaise(action)
          }

          val elems: Var[Maybe[Widget.ElementT]] = Var(None)

          new AppliedWidget[V2] {
            override private[pye] final val value: IO[V2] =
              IO.wrapEffect { leaf.valueF(leaf.lens.get(stateVar.value)) }
            override private[pye] final val current: IO[Maybe[Widget.ElementT]] =
              IO { elems.value }
            override private[pye] final val getElementsAndUpdate: IO[Widget.ElementT] =
              for {
                newElems <- IO { leaf.elementF(leafRH)(leaf.lens.get(stateVar.value)) }
                _ <- elems.value = newElems.some
              } yield newElems
          }
        case mapV: Widget.MapV[V2, S, A2] =>
          val child: AppliedWidget[mapV.V1] = convert(mapV.w, parentRaiseHandler)

          new AppliedWidget[V2] {
            override private[pye] final val value: IO[V2] =
              IO.wrapEffect { mapV.f(child.value.runSync) }
            override private[pye] final val current: IO[Maybe[Widget.ElementT]] =
              child.current
            override private[pye] final val getElementsAndUpdate: IO[Widget.ElementT] =
              child.getElementsAndUpdate
          }
        case mapA: Widget.MapA[V2, S, A2] =>
          Pointer
            .withSelf[AppliedWidget[V2]] { ptr =>
              val mappedRH: RaiseHandler[S, mapA.A1] = {
                case sou: Raise.StandardOrUpdate[S] =>
                  parentRaiseHandler._handleRaise(sou)
                case action: Raise.Action[mapA.A1] =>
                  for {
                    raises <- mapA.f(stateVar.value, ptr.value.value.runSync, action.action)
                    _ <- parentRaiseHandler._handleRaises(raises)
                  } yield ()
              }

              Pointer(convert(mapA.w, mappedRH))
            }
            .value
        case apply: Widget.Apply[V2, S, A2] =>
          val w1: AppliedWidget[apply.V1] =
            Pointer
              .withSelf[AppliedWidget[apply.V1]] { ptr =>
                val rh: RaiseHandler[S, A2] = rhCaptureReRender(ptr, parentRaiseHandler, IO {})
                Pointer(convert(apply.w1, rh))
              }
              .value
          val w2: AppliedWidget[apply.V1 => V2] =
            Pointer
              .withSelf[AppliedWidget[apply.V1 => V2]] { ptr =>
                val rh: RaiseHandler[S, A2] = rhCaptureReRender(ptr, parentRaiseHandler, IO {})
                Pointer(convert(apply.w2, rh))
              }
              .value

          new AppliedWidget[V2] {
            override private[pye] final val value: IO[V2] =
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
        case flatMap: Widget.FlatMap[V2, S, A2] =>
          def makeW2(v1: flatMap.V1): AppliedWidget[V2] =
            Pointer
              .withSelf[AppliedWidget[V2]] { ptr =>
                val rh: RaiseHandler[S, A2] = rhCaptureReRender(ptr, parentRaiseHandler, IO {})
                Pointer(convert(flatMap.w2(v1), rh))
              }
              .value

          lazy val w1: AppliedWidget[flatMap.V1] =
            Pointer
              .withSelf[AppliedWidget[flatMap.V1]] { ptr =>
                val rh: RaiseHandler[S, A2] =
                  rhCaptureReRender(
                    ptr,
                    parentRaiseHandler,
                    w2.reRender.map { _ => },
                  )
                Pointer(convert(flatMap.w1, rh))
              }
              .value

          lazy val w2: AppliedWidget[V2] =
            new AppliedWidget[V2] {
              private val inner: Var[Maybe[Widget.ElementT] \/ AppliedWidget[V2]] = Var(None.left)

              override private[pye] final val value: IO[V2] = {
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

          new AppliedWidget[V2] {
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
            override private[pye] final val value: IO[V2] =
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

          Pointer(convert(thisWidget, rh))
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

  // =====|  |=====

  def zoomOut[S2](
      s2Lens: Lens[S2, S],
  ): Widget[V, S2, A] =
    this match {
      case leaf: Widget.Leaf[V, S, A] =>
        new Widget.Leaf[V, S2, A] {
          override private[pye] final type LeafS = leaf.LeafS
          override private[pye] final type LeafA = leaf.LeafA
          override private[pye] final val lens: Lens[S2, LeafS] = s2Lens.andThen(leaf.lens)
          override private[pye] final val elementF: RaiseHandler[LeafS, LeafA] => LeafS => Widget.ElementT = leaf.elementF
          override private[pye] final val valueF: LeafS => ?[V] = leaf.valueF
        }
      case mapV: Widget.MapV[V, S, A] =>
        new Widget.MapV[V, S2, A] {
          override private[pye] final type V1 = mapV.V1
          override private[pye] final val w: Widget[V1, S2, A] = mapV.w.zoomOut(s2Lens)
          override private[pye] final val f: ?[V1] => ?[V] = mapV.f
        }
      case mapA: Widget.MapA[V, S, A] =>
        new Widget.MapA[V, S2, A] {
          override private[pye] final type A1 = mapA.A1
          override private[pye] final val w: Widget[V, S2, A1] = mapA.w.zoomOut(s2Lens)
          override private[pye] final val f: (S2, ?[V], A1) => AsyncIO[List[Raise[S2, A]]] = { (s2, v, a) =>
            mapA.f(s2Lens.get(s2), v, a).map {
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
      case apply: Widget.Apply[V, S, A] =>
        new Widget.Apply[V, S2, A] {
          override private[pye] final type V1 = apply.V1
          override private[pye] final val w1: Widget[V1, S2, A] = apply.w1.zoomOut(s2Lens)
          override private[pye] final val w2: Widget[V1 => V, S2, A] = apply.w2.zoomOut(s2Lens)
        }
      case flatMap: Widget.FlatMap[V, S, A] =>
        new Widget.FlatMap[V, S2, A] {
          override private[pye] final type V1 = flatMap.V1
          override private[pye] final val w1: Widget[V1, S2, A] = flatMap.w1.zoomOut(s2Lens)
          override private[pye] final val w2: V1 => Widget[V, S2, A] = flatMap.w2(_).zoomOut(s2Lens)
        }
    }

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

  sealed trait Leaf[V, S, +A] extends Widget[V, S, A] {
    private[pye] type LeafS
    private[pye] type LeafA
    private[pye] val lens: Lens[S, LeafS]
    private[pye] val elementF: RaiseHandler[LeafS, LeafA] => LeafS => Widget.ElementT
    private[pye] val valueF: LeafS => ?[V]
  }

  sealed trait MapV[V, S, +A] extends Widget[V, S, A] {
    private[pye] type V1
    private[pye] val w: Widget[V1, S, A]
    private[pye] val f: ?[V1] => ?[V]
  }

  sealed trait MapA[V, S, +A] extends Widget[V, S, A] {
    private[pye] type A1
    private[pye] val w: Widget[V, S, A1]
    private[pye] val f: (S, ?[V], A1) => AsyncIO[List[Raise[S, A]]]
  }

  sealed trait Apply[V, S, +A] extends Widget[V, S, A] {
    private[pye] type V1
    private[pye] val w1: Widget[V1, S, A]
    private[pye] val w2: Widget[V1 => V, S, A]
  }

  sealed trait FlatMap[V, S, +A] extends Widget[V, S, A] {
    private[pye] type V1
    private[pye] val w1: Widget[V1, S, A]
    private[pye] val w2: V1 => Widget[V, S, A]
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
