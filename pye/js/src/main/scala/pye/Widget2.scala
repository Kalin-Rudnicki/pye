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

sealed trait Raise2[+S, +A]
object Raise2 {
  sealed trait StandardOrUpdate[+S] extends Raise2[S, Nothing]

  final case class UpdateState[S](
      update: S => S,
      reRender: Boolean, // TODO (KR) : = true
  ) extends Raise2.StandardOrUpdate[S]

  sealed trait Standard extends Raise2.StandardOrUpdate[Nothing]
  final case class DisplayMessage(
      message: String,
      modifiers: Seq[Modifier],
      timeout: Maybe[Int],
      causeId: Maybe[String],
  ) extends Standard
  sealed trait History extends Standard
  object History {
    final case class Push(page: Page[_]) extends History
    final case class Replace(page: Page[_]) extends History
    final case class Go(delta: Int) extends History

    val Forward: Go = Go(1)
    val Pop: Go = Go(-1)
  }

  final case class Action[+A](action: A) extends Raise2[Nothing, A]
}

trait RaiseHandler2[S, -A] {

  private[pye] def _handleRaise(raise: Raise2[S, A]): AsyncIO[Unit]
  private[pye] def _handleRaises(raises: List[Raise2[S, A]]): AsyncIO[Unit] =
    AsyncIO.runSequentially(raises.map(_handleRaise)).map { _ => }

  def handleRaise(r0: Raise2[S, A], rN: Raise2[S, A]*): Unit =
    handleRaises(r0 :: rN.toList)
  def handleRaises(raises: List[Raise2[S, A]]): Unit =
    _handleRaises(raises).runAndShowErrors()

  def handleAction(a0: A, aN: A*): Unit =
    handleActions(a0 :: aN.toList)
  def handleActions(actions: List[A]): Unit =
    handleRaises(actions.map(Raise2.Action(_)))

}

sealed trait Widget2[V, S, +A] { thisWidget =>

  def render(
      handleActions: A => AsyncIO[List[Raise2.StandardOrUpdate[S]]],
  )(
      initialState: S,
  ): Modifier = {
    val stateVar: Var[S] = Var(initialState)

    def rhCaptureReRender[_V, A2](
        w: Pointer[AppliedWidget[_V]],
        parentRH: RaiseHandler2[S, A2],
        afterUpdate: IO[Unit],
    ): RaiseHandler2[S, A2] = {
      case sou: Raise2.StandardOrUpdate[S] =>
        sou match {
          case standard: Raise2.Standard =>
            parentRH._handleRaise(standard)
          case update: Raise2.UpdateState[S] =>
            for {
              _ <- parentRH._handleRaise(Raise2.UpdateState[S](update.update, false))
              _ <- update.reRender ? AsyncIO.wrapIO(w.value.reRender).map { _ => } | AsyncIO {}
              _ <- AsyncIO.wrapIO(afterUpdate)
            } yield ()
        }
      case action: Raise2.Action[A] =>
        parentRH._handleRaise(action)
    }

    def convert[V2, A2](
        widget: Widget2[V2, S, A2],
        parentRaiseHandler: RaiseHandler2[S, A2],
    ): AppliedWidget[V2] = {
      widget match {
        case leaf: Widget2.Leaf[V2, S, A2] =>
          // NOTE : Trusting the types that by the time the RaiseHandler gets here, it has the correct `A` type
          val pRH: RaiseHandler2[S, leaf.LeafA] =
            parentRaiseHandler.asInstanceOf[RaiseHandler2[S, leaf.LeafA]]

          val leafRH: RaiseHandler2[leaf.LeafS, leaf.LeafA] = {
            case sou: Raise2.StandardOrUpdate[leaf.LeafS] =>
              sou match {
                case standard: Raise2.Standard =>
                  pRH._handleRaise(standard)
                case update: Raise2.UpdateState[leaf.LeafS] =>
                  pRH._handleRaise(Raise2.UpdateState[S](leaf.lens.modify(update.update), update.reRender))
              }
            case action: Raise2.Action[leaf.LeafA] =>
              pRH._handleRaise(action)
          }

          val elems: Var[Maybe[Widget2.ElementT]] = Var(None)

          new AppliedWidget[V2] {
            override private[pye] final val value: IO[V2] =
              IO.wrapEffect { leaf.valueF(leaf.lens.get(stateVar.value)) }
            override private[pye] final val current: IO[Maybe[Widget2.ElementT]] =
              IO { elems.value }
            override private[pye] final val getElementsAndUpdate: IO[Widget2.ElementT] =
              for {
                newElems <- IO { leaf.elementF(leafRH)(leaf.lens.get(stateVar.value)) }
                _ <- elems.value = newElems.some
              } yield newElems
          }
        case mapV: Widget2.MapV[V2, S, A2] =>
          val child: AppliedWidget[mapV.V1] = convert(mapV.w, parentRaiseHandler)

          new AppliedWidget[V2] {
            override private[pye] final val value: IO[V2] =
              IO.wrapEffect { mapV.f(child.value.runSync) }
            override private[pye] final val current: IO[Maybe[Widget2.ElementT]] =
              child.current
            override private[pye] final val getElementsAndUpdate: IO[Widget2.ElementT] =
              child.getElementsAndUpdate
          }
        case mapA: Widget2.MapA[V2, S, A2] =>
          Pointer
            .withSelf[AppliedWidget[V2]] { ptr =>
              val mappedRH: RaiseHandler2[S, mapA.A1] = {
                case sou: Raise2.StandardOrUpdate[S] =>
                  parentRaiseHandler._handleRaise(sou)
                case action: Raise2.Action[mapA.A1] =>
                  for {
                    raises <- mapA.f(stateVar.value, ptr.value.value.runSync, action.action)
                    _ <- parentRaiseHandler._handleRaises(raises)
                  } yield ()
              }

              Pointer(convert(mapA.w, mappedRH))
            }
            .value
        case apply: Widget2.Apply[V2, S, A2] =>
          val w1: AppliedWidget[apply.V1] =
            Pointer
              .withSelf[AppliedWidget[apply.V1]] { ptr =>
                val rh: RaiseHandler2[S, A2] = rhCaptureReRender(ptr, parentRaiseHandler, IO {})
                Pointer(convert(apply.w1, rh))
              }
              .value
          val w2: AppliedWidget[apply.V1 => V2] =
            Pointer
              .withSelf[AppliedWidget[apply.V1 => V2]] { ptr =>
                val rh: RaiseHandler2[S, A2] = rhCaptureReRender(ptr, parentRaiseHandler, IO {})
                Pointer(convert(apply.w2, rh))
              }
              .value

          new AppliedWidget[V2] {
            override private[pye] final val value: IO[V2] =
              w1.value.apply(w2.value)
            override private[pye] final val current: IO[Maybe[Widget2.ElementT]] =
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
            override private[pye] final val getElementsAndUpdate: IO[Widget2.ElementT] =
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
        case flatMap: Widget2.FlatMap[V2, S, A2] =>
          def makeW2(v1: flatMap.V1): AppliedWidget[V2] =
            Pointer
              .withSelf[AppliedWidget[V2]] { ptr =>
                val rh: RaiseHandler2[S, A2] = rhCaptureReRender(ptr, parentRaiseHandler, IO {})
                Pointer(convert(flatMap.w2(v1), rh))
              }
              .value

          lazy val w1: AppliedWidget[flatMap.V1] =
            Pointer
              .withSelf[AppliedWidget[flatMap.V1]] { ptr =>
                val rh: RaiseHandler2[S, A2] =
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
              private val inner: Var[Maybe[Widget2.ElementT] \/ AppliedWidget[V2]] = Var(None.left)

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
              override private[pye] final val current: IO[Maybe[Widget2.ElementT]] = {
                for {
                  evalInner <- IO { inner.value }
                  elems <- evalInner match {
                    case Right(w) => w.current
                    case Left(e)  => IO { e }
                  }
                } yield elems
              }
              override private[pye] final val getElementsAndUpdate: IO[Widget2.ElementT] = {
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
            override private[pye] final val current: IO[Maybe[Widget2.ElementT]] =
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
            override private[pye] final val getElementsAndUpdate: IO[Widget2.ElementT] =
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
          val rh: RaiseHandler2[S, A] = { raise =>
            def handleStandardOrUpdate(sou: Raise2.StandardOrUpdate[S]): AsyncIO[Unit] =
              sou match {
                case standard: Raise2.Standard =>
                  standard match {
                    case msg: Raise2.DisplayMessage =>
                      AsyncIO { displayMessage2(msg) }
                    case history: Raise2.History =>
                      history match {
                        case Raise2.History.Push(page)    => page._push()
                        case Raise2.History.Replace(page) => page._replace()
                        case Raise2.History.Go(delta)     => AsyncIO { window.history.go(delta) }
                      }
                  }
                case Raise2.UpdateState(update, reRender) =>
                  for {
                    _ <- AsyncIO.wrapIO(stateVar.value = update(stateVar.value))
                    _ <- AsyncIO.wrapIO { reRender.maybe(ptr.value.reRender).traverse }
                  } yield ()
              }

            raise match {
              case sou: Raise2.StandardOrUpdate[S] =>
                handleStandardOrUpdate(sou)
              case Raise2.Action(action) =>
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

  // TODO (KR) : Move to Implicit that limits `A` to `Nothing` (?)
  def renderNoAction(
      initialState: S,
  ): Modifier =
    render(_ => Nil.pure[AsyncIO])(initialState)

  // =====|  |=====

  // TODO (KR) : Possibly make more efficient
  def mapValue_?[V2](
      mapF: ?[V] => ?[V2],
  ): Widget2[V2, S, A] =
    new Widget2.MapV[V2, S, A] {
      override final type V1 = V
      override final val w: Widget2[V1, S, A] = thisWidget
      override final val f: ?[V1] => ?[V2] = mapF
    }

  // TODO (KR) : Possibly make more efficient
  def mapAction[A0 >: A, A2](
      mapF: (S, ?[V], A0) => AsyncIO[List[Raise2[S, A2]]],
  ): Widget2[V, S, A2] =
    new Widget2.MapA[V, S, A2] {
      override final type A1 = A0
      override final val w: Widget2[V, S, A1] = thisWidget
      override final val f: (S, ?[V], A0) => AsyncIO[List[Raise2[S, A2]]] = mapF
    }

  // TODO (KR) : Possibly make more efficient
  private def _apply[A0 >: A, V2](
      w: Widget2[V => V2, S, A0],
  ): Widget2[V2, S, A0] =
    new Widget2.Apply[V2, S, A0] {
      override final type V1 = V
      override final val w1: Widget2[V1, S, A0] = thisWidget
      override final val w2: Widget2[V1 => V2, S, A0] = w
    }

  // TODO (KR) : Possibly make more efficient
  private def _flatMap[A0 >: A, V2](
      wF: V => Widget2[V2, S, A0],
  ): Widget2[V2, S, A0] =
    new Widget2.FlatMap[V2, S, A0] {
      override final type V1 = V
      override final val w1: Widget2[V1, S, A0] = thisWidget
      override final val w2: V => Widget2[V2, S, A0] = wF
    }

  // =====|  |=====

  def zoomOut[S2](
      s2Lens: Lens[S2, S],
  ): Widget2[V, S2, A] =
    this match {
      case leaf: Widget2.Leaf[V, S, A] =>
        new Widget2.Leaf[V, S2, A] {
          override private[pye] final type LeafS = leaf.LeafS
          override private[pye] final type LeafA = leaf.LeafA
          override private[pye] final val lens: Lens[S2, LeafS] = s2Lens.andThen(leaf.lens)
          override private[pye] final val elementF: RaiseHandler2[LeafS, LeafA] => LeafS => Widget2.ElementT = leaf.elementF
          override private[pye] final val valueF: LeafS => ?[V] = leaf.valueF
        }
      case mapV: Widget2.MapV[V, S, A] =>
        new Widget2.MapV[V, S2, A] {
          override private[pye] final type V1 = mapV.V1
          override private[pye] final val w: Widget2[V1, S2, A] = mapV.w.zoomOut(s2Lens)
          override private[pye] final val f: ?[V1] => ?[V] = mapV.f
        }
      case mapA: Widget2.MapA[V, S, A] =>
        new Widget2.MapA[V, S2, A] {
          override private[pye] final type A1 = mapA.A1
          override private[pye] final val w: Widget2[V, S2, A1] = mapA.w.zoomOut(s2Lens)
          override private[pye] final val f: (S2, ?[V], A1) => AsyncIO[List[Raise2[S2, A]]] = { (s2, v, a) =>
            mapA.f(s2Lens.get(s2), v, a).map {
              _.map {
                case action: Raise2.Action[A] => action
                case sou: Raise2.StandardOrUpdate[S] =>
                  sou match {
                    case standard: Raise2.Standard     => standard
                    case update: Raise2.UpdateState[S] => Raise2.UpdateState(s2Lens.modify(update.update), update.reRender)
                  }
              }
            }
          }
        }
      case apply: Widget2.Apply[V, S, A] =>
        new Widget2.Apply[V, S2, A] {
          override private[pye] final type V1 = apply.V1
          override private[pye] final val w1: Widget2[V1, S2, A] = apply.w1.zoomOut(s2Lens)
          override private[pye] final val w2: Widget2[V1 => V, S2, A] = apply.w2.zoomOut(s2Lens)
        }
      case flatMap: Widget2.FlatMap[V, S, A] =>
        new Widget2.FlatMap[V, S2, A] {
          override private[pye] final type V1 = flatMap.V1
          override private[pye] final val w1: Widget2[V1, S2, A] = flatMap.w1.zoomOut(s2Lens)
          override private[pye] final val w2: V1 => Widget2[V, S2, A] = flatMap.w2(_).zoomOut(s2Lens)
        }
    }

  // =====|  |=====

  def mapValue[V2](
      mapF: V => V2,
  ): Widget2[V2, S, A] =
    mapValue_?(_.map(mapF))

  def flatMapValue[V2](
      mapF: V => ?[V2],
  ): Widget2[V2, S, A] =
    mapValue_?(_.flatMap(mapF))

}
object Widget2 {

  type ElementT = NonEmptyList[Element]
  type ElemT = Element

  // =====|  |=====

  def builder: Builder1 = new Builder1

  final class Builder1 private[Widget2] {

    def withState[S]: Builder2[S] = new Builder2[S]

    def element[S, A](elem: => Widget.ElemT): Widget2[Unit, S, A] =
      withState[S].withAction[A].element(elem).noValue

  }

  final class Builder2[S] private[Widget2] {

    def withAction[A]: Builder3[S, A] = new Builder3[S, A]
    def submitAction: Builder3[S, CommonRaise.Submit.type] = withAction[CommonRaise.Submit.type]
    def submitOrAction[O]: Builder3[S, CommonRaise.SubmitOr[O]] = withAction[CommonRaise.SubmitOr[O]]

    def noAction: Builder3[S, Nothing] = withAction[Nothing]

  }

  final class Builder3[S, A] private[Widget2] {

    def rsElements(elementF: RaiseHandler2[S, A] => S => Widget2.ElementT): Builder4[S, A] =
      new Builder4[S, A](elementF)
    def rElements(elementF: RaiseHandler2[S, A] => Widget2.ElementT): Builder4[S, A] =
      rsElements(r => _ => elementF(r))
    def sElements(elementF: S => Widget2.ElementT): Builder4[S, A] =
      rsElements(_ => elementF)
    def elements(elementF: => Widget2.ElementT): Builder4[S, A] =
      rsElements(_ => _ => elementF)

    def rsElement(elementF: RaiseHandler2[S, A] => S => Widget2.ElemT): Builder4[S, A] =
      rsElements(r => s => NonEmptyList(elementF(r)(s), Nil))
    def rElement(elementF: RaiseHandler2[S, A] => Widget2.ElemT): Builder4[S, A] =
      rsElement(r => _ => elementF(r))
    def sElement(elementF: S => Widget2.ElemT): Builder4[S, A] =
      rsElement(_ => elementF)
    def element(elementF: => Widget2.ElemT): Builder4[S, A] =
      rsElement(_ => _ => elementF)

  }

  final class Builder4[S, A] private[Widget2] (elementF: RaiseHandler2[S, A] => S => Widget2.ElementT) {

    def withValue[V](valueF: S => ?[V]): Widget2[V, S, A] = {
      val _elementF = elementF
      val _valueF = valueF

      new Widget2.Leaf[V, S, A] {
        override private[pye] final type LeafS = S
        override private[pye] final type LeafA = A
        override private[pye] final val lens: Lens[S, S] = Lens.id[S]
        override private[pye] final val elementF: RaiseHandler2[S, A] => S => ElementT = _elementF
        override private[pye] final val valueF: S => ?[V] = _valueF
      }
    }

    def noValue: Widget2[Unit, S, A] = withValue[Unit](_ => ().pure[?])

  }

  // =====|  |=====

  private[pye] def replaceNodes(oldElems: Widget2.ElementT, newElems: Widget2.ElementT): IO[Unit] = {
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

  sealed trait Leaf[V, S, +A] extends Widget2[V, S, A] {
    private[pye] type LeafS
    private[pye] type LeafA
    private[pye] val lens: Lens[S, LeafS]
    private[pye] val elementF: RaiseHandler2[LeafS, LeafA] => LeafS => Widget2.ElementT
    private[pye] val valueF: LeafS => ?[V]
  }

  sealed trait MapV[V, S, +A] extends Widget2[V, S, A] {
    private[pye] type V1
    private[pye] val w: Widget2[V1, S, A]
    private[pye] val f: ?[V1] => ?[V]
  }

  sealed trait MapA[V, S, +A] extends Widget2[V, S, A] {
    private[pye] type A1
    private[pye] val w: Widget2[V, S, A1]
    private[pye] val f: (S, ?[V], A1) => AsyncIO[List[Raise2[S, A]]]
  }

  sealed trait Apply[V, S, +A] extends Widget2[V, S, A] {
    private[pye] type V1
    private[pye] val w1: Widget2[V1, S, A]
    private[pye] val w2: Widget2[V1 => V, S, A]
  }

  sealed trait FlatMap[V, S, +A] extends Widget2[V, S, A] {
    private[pye] type V1
    private[pye] val w1: Widget2[V1, S, A]
    private[pye] val w2: V1 => Widget2[V, S, A]
  }

  // =====|  |=====

  type Projection[S, A] = { type P[V] = Widget2[V, S, A] }

  implicit def widgetMonad[S, A]: Monad[Widget2.Projection[S, A]#P] =
    new Monad[Projection[S, A]#P] {

      override def map[V1, V2](t: Widget2[V1, S, A], f: V1 => V2): Widget2[V2, S, A] =
        t.mapValue(f)

      override def apply[V1, V2](t: Widget2[V1, S, A], f: Widget2[V1 => V2, S, A]): Widget2[V2, S, A] =
        t._apply(f)

      // NOTE : This should really never be used, but is needed for Applicative
      //      : It would also be nice if ElementT could be an empty List,
      //      : but at least for the time being, NonEmptyList is necessary
      override def pure[V1](a: => V1): Widget2[V1, S, A] = {
        // TODO (KR) :
        ???
      }

      override def flatMap[V1, V2](t: Widget2[V1, S, A], f: V1 => Widget2[V2, S, A]): Widget2[V2, S, A] =
        t._flatMap(f)

    }

  implicit def widgetTraverseList[S, A]: Traverse[List, Widget2.Projection[S, A]#P] =
    new Traverse[List, Widget2.Projection[S, A]#P] {

      override def traverse[T](t: List[Widget2[T, S, A]]): Widget2[List[T], S, A] = ???

    }

}

sealed trait AppliedWidget[V] {

  private[pye] val current: IO[Maybe[Widget2.ElementT]]
  private[pye] val value: IO[V]
  private[pye] val getElementsAndUpdate: IO[Widget2.ElementT]

  private[pye] final val reRender: IO[Widget2.ElementT] =
    for {
      // TODO (KR) : There is a bug here...
      //           : Somehow adding this blank `IO {}` fixes it
      //           : (there are 2 of these)
      _ <- IO {}

      mOldElements <- current
      newElements <- getElementsAndUpdate
      _ <- mOldElements.map(Widget2.replaceNodes(_, newElements)).traverse
    } yield newElements

}
