package pye

import java.util.UUID

import monocle.Lens
import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import pye.Widget2.ElementT

sealed trait Raise2[+S, +A]
object Raise2 {
  sealed trait StandardOrUpdate[+S] extends Raise2[S, Nothing]

  final case class UpdateState[S](
      update: S => S,
      reRender: Boolean, // TODO (KR) : = true
  ) extends Raise2.StandardOrUpdate[S]

  sealed trait Standard extends Raise2.StandardOrUpdate[Nothing]
  // TODO (KR) :

  final case class Action[+A](action: A) extends Raise2[Nothing, A]
}

trait RaiseHandler2[S, -A] {

  def _handleRaise(raise: Raise2[S, A]): AsyncIO[Unit]
  def _handleRaises(raises: List[Raise2[S, A]]): AsyncIO[Unit] =
    AsyncIO.runSequentially(raises.map(_handleRaise)).map { _ => }

}

sealed trait Widget2[V, S, +A] { thisWidget =>
  type Value = V
  type State = S
  type Action <: A

  def render(
      handleActions: A => AsyncIO[List[Raise2.StandardOrUpdate[S]]],
  )(
      initialState: S,
  ): Modifier = {
    val stateVar: Var[S] = Var(initialState)

    def rhCaptureReRender[_V, A2](
        w: Pointer[AppliedWidget[_V]],
        parentRH: RaiseHandler2[S, A2],
        afterUpdate: IO[Unit] = IO {},
    ): RaiseHandler2[S, A2] = {
      case sou: Raise2.StandardOrUpdate[S] =>
        sou match {
          case standard: Raise2.Standard =>
            parentRH._handleRaise(standard)
          case update: Raise2.UpdateState[S] =>
            if (update.reRender)
              for {
                _ <- parentRH._handleRaise(update)
                _ <- AsyncIO.wrapIO(w.value.reRender)
                _ <- AsyncIO.wrapIO(afterUpdate)
              } yield ()
            else
              parentRH._handleRaise(update)
        }
      case action: Raise2.Action[A] =>
        parentRH._handleRaise(action)
    }

    def convert[V2, A2](
        widget: Widget2[V2, S, A2],
        parentRaiseHandler: RaiseHandler2[S, A2],
    ): AppliedWidget[V2] =
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

          val eV = Var.`null`[Widget2.ElementT]

          val aW =
            new AppliedWidget[V2] {
              override private[pye] final val value: IO[V2] =
                IO.wrapEffect { leaf.valueF(leaf.lens.get(stateVar.value)) }
              override private[pye] final val makeElements: () => Widget2.ElementT =
                () => leaf.elementF(leafRH)(leaf.lens.get(stateVar.value)) // TODO (KR) :
              override private[pye] final val reRender: IO[Widget2.ElementT] =
                for {
                  newElems <- IO { makeElements() }
                  _ <- Widget2.replaceNodes(eV.value, newElems)
                  _ <- eV.value = newElems
                } yield newElems
            }

          aW
        case mapV: Widget2.MapV[V2, S, A2] =>
          val child: AppliedWidget[mapV.V1] = convert(mapV.w, parentRaiseHandler)

          new AppliedWidget[V2] {
            override private[pye] final val value: IO[V2] =
              IO.wrapEffect { mapV.f(child.value.runSync) }
            override private[pye] final val makeElements: () => Widget2.ElementT =
              child.makeElements
            override private[pye] final val reRender: IO[Widget2.ElementT] =
              child.reRender
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
                val rh: RaiseHandler2[S, A2] = rhCaptureReRender(ptr, parentRaiseHandler)
                Pointer(convert(apply.w1, rh))
              }
              .value
          val w2: AppliedWidget[apply.V1 => V2] =
            Pointer
              .withSelf[AppliedWidget[apply.V1 => V2]] { ptr =>
                val rh: RaiseHandler2[S, A2] = rhCaptureReRender(ptr, parentRaiseHandler)
                Pointer(convert(apply.w2, rh))
              }
              .value

          new AppliedWidget[V2] {
            override private[pye] final val value: IO[V2] =
              IO.wrapEffect { w1.value.runSync.apply(w2.value.runSync) }
            override private[pye] final val makeElements: () => Widget2.ElementT =
              () => NonEmptyList.nel(w1.makeElements(), w2.makeElements()).flatten
            override private[pye] final val reRender: IO[Widget2.ElementT] =
              for {
                w1E <- w1.reRender
                w2E <- w2.reRender
              } yield NonEmptyList.nel(w1E, w2E).flatten
          }
        case flatMap: Widget2.FlatMap[V2, S, A2] =>
          def makeW2(v1: flatMap.V1): AppliedWidget[V2] =
            Pointer
              .withSelf[AppliedWidget[V2]] { ptr =>
                val rh: RaiseHandler2[S, A2] = rhCaptureReRender(ptr, parentRaiseHandler)
                Pointer(convert(flatMap.w2(v1), rh))
              }
              .value

          val w1: AppliedWidget[flatMap.V1] =
            Pointer
              .withSelf[AppliedWidget[flatMap.V1]] { ptr =>
                val rh: RaiseHandler2[S, A2] = rhCaptureReRender(ptr, parentRaiseHandler)
                Pointer(convert(flatMap.w1, rh))
              }
              .value
          val w2: AppliedWidget[V2] =
            new AppliedWidget[V2] {
              override private[pye] final val value: IO[V2] =
                w1.value.flatMap(makeW2(_).value)
              override private[pye] final val makeElements: () => Widget2.ElementT = { () =>
                w1.value.runSync match {
                  case Alive(r) =>
                    makeW2(r).makeElements()
                  case Dead(_) =>
                    NonEmptyList.nel(span(id := UUID.randomUUID.toString).render)
                }
              }
              override private[pye] final val reRender: IO[Widget2.ElementT] =
                ??? // TODO (KR) :
            }

          /*
            NOTE : Current approach is not going to work
                 : There is no way to have the second part of flatMap know how to reRender
           */

          new AppliedWidget[V2] {
            override private[pye] final val value: IO[V2] =
              w2.value
            override private[pye] final val makeElements: () => Widget2.ElementT =
              w2.makeElements
            override private[pye] final val reRender: IO[Widget2.ElementT] =
              for {
                w1E <- w1.reRender
                w2E <- w2.reRender
              } yield NonEmptyList.nel(w1E, w2E).flatten
          }
      }

    val appliedWidget: AppliedWidget[V] =
      convert[V, A](
        widget = thisWidget,
        parentRaiseHandler = { raise =>
          def handleStandardOrUpdate(sou: Raise2.StandardOrUpdate[S]): AsyncIO[Unit] =
            sou match {
              case standard: Raise2.Standard =>
                // TODO (KR) :
                ???
              case Raise2.UpdateState(update, reRender) =>
                for {
                  _ <- AsyncIO.wrapIO(stateVar.value = update(stateVar.value))
                  // TODO (KR) : reRender
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
        }, // TODO (KR) :
      )

    // TODO (KR) : make sure that appliedWidget.elements.value is properly populated
    appliedWidget.makeElements().toList
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
  def _apply[A0 >: A, V2](
      w: Widget2[V => V2, S, A0],
  ): Widget2[V2, S, A0] =
    new Widget2.Apply[V2, S, A0] {
      override final type V1 = V
      override final val w1: Widget2[V1, S, A0] = thisWidget
      override final val w2: Widget2[V1 => V2, S, A0] = w
    }

  // TODO (KR) : Possibly make more efficient
  def _flatMap[A0 >: A, V2](
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
          override private[pye] final val elementF: RaiseHandler2[LeafS, LeafA] => LeafS => ElementT = leaf.elementF
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

  // =====|  |=====

  private def replaceNodes(oldElems: Widget2.ElementT, newElems: Widget2.ElementT): IO[Unit] =
    Maybe(oldElems)
      .map { elems =>
        ??? : IO[Unit]
      }
      .traverse
      .map { _ => }

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

}

sealed trait AppliedWidget[V] {

  private[pye] val value: IO[V]
  private[pye] val makeElements: () => Widget2.ElementT
  private[pye] val reRender: IO[Widget2.ElementT]

}
