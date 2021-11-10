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

trait Widget[V, S, +A] { thisWidget =>

  final def mapValue_?[V2](
      mapF: ?[V] => ?[V2],
  ): Widget[V2, S, A] =
    new Widget.MapV[V2, S, A] {
      override final type V1 = V
      override final val w: Widget[V1, S, A] = thisWidget
      override final val f: (S, ?[V1]) => ?[V2] = (_, v) => mapF(v)
    }

  final def mapRaise[A0 >: A, A2](
      mapF: (S, ?[V], Raise[S, A0]) => AsyncIO[List[Raise[S, A2]]],
  ): Widget[V, S, A2] =
    new Widget.MapA[V, S, A2] {
      override final type A1 = A0
      override final val w: Widget[V, S, A1] = thisWidget
      override final val f: (S, ?[V], Raise[S, A0]) => AsyncIO[List[Raise[S, A2]]] = mapF
    }

  final def mapAction[A0 >: A, A2](
      mapF: (S, ?[V], A0) => AsyncIO[List[Raise[S, A2]]],
  ): Widget[V, S, A2] =
    mapRaise[A0, A2] { (s, v, r) =>
      r match {
        case sou: Raise.StandardOrUpdate[S] =>
          List(sou).pure[AsyncIO]
        case action: Raise.Action[A0] =>
          mapF(s, v, action.action)
      }
    }

  private final def _apply[A0 >: A, V2](
      w: Widget[V => V2, S, A0],
  ): Widget[V2, S, A0] =
    new Widget.Apply[V2, S, A0] {
      override final type V1 = V
      override final val w1: Widget[V1, S, A0] = thisWidget
      override final val w2: Widget[V1 => V2, S, A0] = w
    }

  final def wrapped(inElement: Modifier => Widget.ElemT): Widget[V, S, A] =
    rsWrapped[A](_ => _ => inElement)
  final def wrappedElems(inElements: Modifier => Widget.ElementT): Widget[V, S, A] =
    rsWrappedElems[A](_ => _ => inElements)

  def rWrapped[A0 >: A](inElement: RaiseHandler[S, A0] => Modifier => Widget.ElemT): Widget[V, S, A0] =
    rsWrapped[A0](rh => _ => inElement(rh))
  def rWrappedElems[A0 >: A](inElement: RaiseHandler[S, A0] => Modifier => Widget.ElementT): Widget[V, S, A0] =
    rsWrappedElems[A0](rh => _ => inElement(rh))

  final def sWrapped(inElement: S => Modifier => Widget.ElemT): Widget[V, S, A] =
    rsWrapped[A](_ => inElement)
  final def sWrappedElems(inElement: S => Modifier => Widget.ElementT): Widget[V, S, A] =
    rsWrappedElems[A](_ => inElement)

  final def rsWrapped[A0 >: A](inElement: RaiseHandler[S, A0] => S => Modifier => Widget.ElemT): Widget[V, S, A0] =
    rsWrappedElems[A0](rh => s => elems => NonEmptyList.nel(inElement(rh)(s)(elems)))
  final def rsWrappedElems[A0 >: A](inElements: RaiseHandler[S, A0] => S => Modifier => Widget.ElementT): Widget[V, S, A0] =
    new Widget.Wrapped[V, S, A0] {
      override protected final val w: Widget[V, S, A0] = thisWidget
      override protected final val f: RaiseHandler[S, A0] => S => Modifier => Widget.ElementT = inElements
    }

  final def zoomOut[S2](s2Lens: Lens[S2, S]): Widget[V, S2, A] =
    new Widget.ZoomOut[V, S2, A] {
      override protected final type S1 = S
      override protected final val w: Widget[V, S1, A] = thisWidget
      override protected final val lens: Lens[S2, S1] = s2Lens
    }

  final def captureReRender: Widget[V, S, A] = captureReRender(RaiseHandler.ReRender.Nothing)
  final def captureReRender(reRenders: RaiseHandler.ReRender): Widget[V, S, A] =
    new Widget[V, S, A] {
      override val widgetName: String = s"${thisWidget.widgetName}-captureReRender"
      override protected def convertImpl(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V] =
        Pointer
          .withSelf[AppliedWidget[V]] { ptr =>
            val rh: RaiseHandler[S, A] = Widget.rhCaptureReRender(ptr, parentRaiseHandler, reRenders)
            Pointer(thisWidget.convert(rh, getState))
          }
          .value
    }

  def captureTags(tag0: String, tagN: String*): Widget[V, S, A] =
    new Widget[V, S, A] {
      override val widgetName: String = s"${thisWidget.widgetName}-captureTags"
      override protected def convertImpl(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V] =
        Pointer
          .withSelf[AppliedWidget[V]] { ptr =>
            val rh: RaiseHandler[S, A] = Widget.rhCaptureTags(ptr, parentRaiseHandler, (tag0 :: tagN.toList).toSet)
            Pointer(thisWidget.convert(rh, getState))
          }
          .value
    }

  // =====|  |=====

  val widgetName: String

  protected def convertImpl(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V]

  def convert(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V] = {
    PyeLogger.unsafeLog.debug(s"convert[$widgetName]", "convert")
    val res = convertImpl(parentRaiseHandler, getState)
    res.widgetName = widgetName
    res
  }

  // =====|  |=====

  final def mapValueWithState[V2](
      mapF: (S, V) => V2,
  ): Widget[V2, S, A] =
    new Widget.MapV[V2, S, A] {
      override final type V1 = V
      override final val w: Widget[V1, S, A] = thisWidget
      override final val f: (S, ?[V1]) => ?[V2] = (s, v) => v.map(mapF(s, _))
    }

  final def mapValue[V2](
      mapF: V => V2,
  ): Widget[V2, S, A] =
    mapValue_?(_.map(mapF))

  final def asUnit: Widget[Unit, S, A] =
    mapValue { _ => }

  final def ignoreActions: Widget[V, S, Nothing] =
    mapAction[A, Nothing] { (_, _, a) =>
      for {
        _ <- PyeLogger.log.debug(s"Widget ignored action : $a").toAsyncIO
      } yield Nil
    }

  final def flatMapValue[V2](
      mapF: V => ?[V2],
  ): Widget[V2, S, A] =
    mapValue_?(_.flatMap(mapF))

  final def labelled(
      labelText: String,
      decorator: Modifier = Seq.empty[Modifier],
  ): Widget[V, S, A] =
    ado[Widget.Projection[S, A]#P]
      .join(
        Widget.builder.element[S, A] {
          label(PyeS.`pye:label`)(labelText)(decorator).render
        },
        this,
      )
      .mapValue_? {
        case Alive(r) =>
          r._2.pure[?]
        case Dead(errors) =>
          Dead(errors.map(_.mappedMessage(msg => s"$labelText $msg")))
      }

  override def toString: String = widgetName

  // =====|  |=====

  def map[V2](mapF: V => V2): Widget[V2, S, A] = mapValue(mapF)

  def flatMap[V2, A0 >: A](mapF: V => Widget[V2, S, A0]): Widget[V2, S, A0] =
    new Widget.FlatMap[V2, S, A0] {
      override final type V1 = V
      override final val w1: Widget[V1, S, A0] = thisWidget
      override final val w2: V => Widget[V2, S, A0] = mapF
      override final val w2GoesAfter: Boolean = true
    }

  def flatMapBefore[V2, A0 >: A](mapF: V => Widget[V2, S, A0]): Widget[V2, S, A0] =
    new Widget.FlatMap[V2, S, A0] {
      override final type V1 = V
      override final val w1: Widget[V1, S, A0] = thisWidget
      override final val w2: V => Widget[V2, S, A0] = mapF
      override final val w2GoesAfter: Boolean = false
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
        override protected final val elementF: RaiseHandler[S, A] => S => ElementT = _elementF
        override protected final val valueF: S => ?[V] = _valueF
      }
    }

    def noValue: Widget[Unit, S, A] = withValue[Unit](_ => ().pure[?])

  }

  // =====|  |=====

  def placeholderSpan: Widget.ElemT = span(id := UUID.randomUUID.toString, display.none).render

  def rhCaptureReRender[V, S, A](
      w: Pointer[AppliedWidget[V]],
      parentRH: RaiseHandler[S, A],
      reRenders: RaiseHandler.ReRender = RaiseHandler.ReRender.Nothing,
  ): RaiseHandler[S, A] = {
    case sou: Raise.StandardOrUpdate[S] =>
      sou match {
        case standard: Raise.Standard =>
          parentRH._handleRaise(standard)
        case update: Raise.UpdateState[S] =>
          val (usRR, rhRR) = update.reRender.ifForced(w.value, update.childReRenders)

          parentRH._handleRaise(
            Raise.UpdateState[S](
              update.update,
              usRR,
              RaiseHandler.ReRender.merge(
                List(
                  reRenders,
                  rhRR,
                ),
              ),
            ),
          )
      }
    case action: Raise.Action[A] =>
      parentRH._handleRaise(action)
  }

  def rhCaptureTags[V, S, A](
      w: Pointer[AppliedWidget[V]],
      parentRH: RaiseHandler[S, A],
      tags: Set[String],
  ): RaiseHandler[S, A] = {
    case sou: Raise.StandardOrUpdate[S] =>
      sou match {
        case standard: Raise.Standard =>
          parentRH._handleRaise(standard)
        case update: Raise.UpdateState[S] =>
          val (usRR, rhRR) = update.reRender.ifTagged(tags, w.value, update.childReRenders)

          PyeLogger.unsafeLog.detailed(s"<Tag>\n${update.childReRenders}\n$rhRR")

          parentRH._handleRaise(
            Raise.UpdateState[S](
              update.update,
              usRR,
              rhRR,
            ),
          )
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
    protected val elementF: RaiseHandler[S, A] => S => Widget.ElementT
    protected val valueF: S => ?[V]

    override final val widgetName: String = "Leaf"

    override protected final def convertImpl(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V] = {
      new AppliedWidget[V] {
        private val elems: Var[Maybe[Widget.ElementT]] = Var(None)

        override final val valueImpl: IO[V] =
          IO.wrapEffect { thisWidget.valueF(getState()) }
        override final val currentImpl: IO[Maybe[Widget.ElementT]] =
          IO { elems.value }
        override final val getElementsAndUpdateImpl: IO[Widget.ElementT] =
          for {
            newElems <- IO { thisWidget.elementF(parentRaiseHandler)(getState()) }
            _ <- elems.value = newElems.some
          } yield newElems
      }
    }

  }

  sealed trait MapV[V, S, +A] extends Widget[V, S, A] { thisWidget =>
    protected type V1
    protected val w: Widget[V1, S, A]
    protected val f: (S, ?[V1]) => ?[V]

    override final val widgetName: String = "MapV"

    override protected final def convertImpl(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V] = {
      val child: AppliedWidget[thisWidget.V1] = thisWidget.w.convert(parentRaiseHandler, getState)

      new AppliedWidget[V] {
        override final val valueImpl: IO[V] =
          IO.wrapEffect { thisWidget.f(getState(), child.value.runSync) }
        override final val currentImpl: IO[Maybe[Widget.ElementT]] =
          child.current
        override final val getElementsAndUpdateImpl: IO[Widget.ElementT] =
          child.getElementsAndUpdate
      }
    }

  }

  sealed trait MapA[V, S, +A] extends Widget[V, S, A] { thisWidget =>
    protected type A1
    protected val w: Widget[V, S, A1]
    protected val f: (S, ?[V], Raise[S, A1]) => AsyncIO[List[Raise[S, A]]]

    override final val widgetName: String = "MapA"

    override protected final def convertImpl(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V] =
      Pointer
        .withSelf[AppliedWidget[V]] { ptr =>
          val mappedRH: RaiseHandler[S, thisWidget.A1] = { raise =>
            for {
              raises <- f(getState(), ptr.value.value.runSync, raise)
              prr <- parentRaiseHandler._handleRaises(raises)
            } yield prr
          }

          Pointer(thisWidget.w.convert(mappedRH, getState))
        }
        .value

  }

  sealed trait Apply[V, S, +A] extends Widget[V, S, A] { thisWidget =>
    protected type V1
    protected val w1: Widget[V1, S, A]
    protected val w2: Widget[V1 => V, S, A]

    override final val widgetName: String = "Apply"

    override protected final def convertImpl(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V] = {
      val w1: AppliedWidget[thisWidget.V1] =
        thisWidget.w1.captureReRender.convert(parentRaiseHandler, getState)
      val w2: AppliedWidget[thisWidget.V1 => V] =
        thisWidget.w2.captureReRender.convert(parentRaiseHandler, getState)

      new AppliedWidget[V] {
        override final val valueImpl: IO[V] =
          w1.value.apply(w2.value)
        override final val currentImpl: IO[Maybe[Widget.ElementT]] =
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
        override final val getElementsAndUpdateImpl: IO[Widget.ElementT] =
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
    protected type V1
    protected val w1: Widget[V1, S, A]
    protected val w2: V1 => Widget[V, S, A]
    protected val w2GoesAfter: Boolean

    override final val widgetName: String = "FlatMap"

    override protected final def convertImpl(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V] = {
      def makeW2(v1: thisWidget.V1): AppliedWidget[V] =
        thisWidget.w2(v1).captureReRender.convert(parentRaiseHandler, getState)

      lazy val w1: AppliedWidget[thisWidget.V1] =
        thisWidget.w1.captureReRender(RaiseHandler.ReRender(w2)).convert(parentRaiseHandler, getState)

      lazy val w2: AppliedWidget[V] =
        new AppliedWidget[V] {
          private val inner: Var[Maybe[Widget.ElementT] \/ AppliedWidget[V]] = Var(None.left)

          override final val valueImpl: IO[V] = {
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
          override final val currentImpl: IO[Maybe[Widget.ElementT]] = {
            for {
              evalInner <- IO { inner.value }
              elems <- evalInner match {
                case Right(w) => w.current
                case Left(e)  => IO { e }
              }
            } yield elems
          }
          override final val getElementsAndUpdateImpl: IO[Widget.ElementT] = {
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
                    newElem <- IO { Widget.placeholderSpan }
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
        override final val valueImpl: IO[V] =
          w2.value
        override final val currentImpl: IO[Maybe[Widget.ElementT]] =
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
        override final val getElementsAndUpdateImpl: IO[Widget.ElementT] =
          ado[IO]
            .join(
              w1.getElementsAndUpdate,
              w2.getElementsAndUpdate,
            )
            .map {
              case (w1E, w2E) =>
                val (elems1, elems2) =
                  if (w2GoesAfter) (w1E, w2E)
                  else (w2E, w1E)

                NonEmptyList.nel(elems1, elems2).flatten
            }
      }
    }

  }

  // TODO (KR) : Implement convert here...
  sealed trait ZoomOut[V, S, +A] extends Widget[V, S, A] { thisWidget =>
    protected type S1
    protected val w: Widget[V, S1, A]
    protected val lens: Lens[S, S1]

    override final val widgetName: String = "ZoomOut"

    override protected final def convertImpl(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V] = {
      val rh: RaiseHandler[S1, A] = {
        case sou: Raise.StandardOrUpdate[S1] =>
          sou match {
            case update: Raise.UpdateState[S1] =>
              parentRaiseHandler._handleRaise(
                update.mapUpdate[S] { uF => lens.modify(uF) },
              )
            case standard: Raise.Standard =>
              parentRaiseHandler._handleRaise(standard)
          }
        case action: Raise.Action[A] =>
          parentRaiseHandler._handleRaise(action)
      }

      w.convert(rh, () => lens.get(getState()))
    }

  }

  sealed trait Wrapped[V, S, +A] extends Widget[V, S, A] { thisWidget =>
    protected val w: Widget[V, S, A]
    protected val f: RaiseHandler[S, A] => S => Modifier => Widget.ElementT

    override final val widgetName: String = "Wrapped"

    override protected final def convertImpl(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V] = {
      val child: AppliedWidget[V] =
        thisWidget.w.captureReRender.convert(parentRaiseHandler, getState)

      new AppliedWidget[V] {
        private val elems: Var[Maybe[Widget.ElementT]] = Var(None)

        override final val valueImpl: IO[V] =
          child.value
        override final val currentImpl: IO[Maybe[Widget.ElementT]] =
          IO { elems.value }
        override final val getElementsAndUpdateImpl: IO[Widget.ElementT] =
          for {
            childElems <- child.getElementsAndUpdate
            myElems = thisWidget.f(parentRaiseHandler)(getState())(childElems.toList)
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
        t.flatMap(f)

    }

  implicit def widgetTraverseList[S, A]: Traverse[List, Widget.Projection[S, A]#P] =
    new Traverse[List, Widget.Projection[S, A]#P] {

      override def traverse[T](t: List[Widget[T, S, A]]): Widget[List[T], S, A] =
        new Widget[List[T], S, A] {
          override final val widgetName: String = "TraverseList"

          override protected def convertImpl(
              parentRaiseHandler: RaiseHandler[S, A],
              getState: () => S,
          ): AppliedWidget[List[T]] =
            t.toNel match {
              case Some(nel) =>
                type MyT[V] = Widget[V, S, A]
                (nel: NonEmptyList[MyT[T]]).traverse.map(_.toList).convert(parentRaiseHandler, getState)
              case None =>
                new AppliedWidget[List[T]] {
                  private val elems: Var[Maybe[Widget.ElementT]] = Var(None)

                  // TODO (KR) : This seems wrong...
                  override final val valueImpl: IO[List[T]] = IO { Nil }
                  override final val currentImpl: IO[Maybe[Widget.ElementT]] = IO { elems.value }
                  override final val getElementsAndUpdateImpl: IO[ElementT] =
                    for {
                      newElems <- IO { NonEmptyList.nel(Widget.placeholderSpan) }
                      _ <- elems.value = newElems.some
                    } yield newElems
                }
            }
        }

    }

  implicit def widgetTraverseNonEmptyList[S, A]: Traverse[NonEmptyList, Widget.Projection[S, A]#P] =
    new Traverse[NonEmptyList, Widget.Projection[S, A]#P] {

      override def traverse[T](t: NonEmptyList[Widget[T, S, A]]): Widget[NonEmptyList[T], S, A] =
        new Widget[NonEmptyList[T], S, A] {
          override final val widgetName: String = "TraverseNonEmptyList"

          override protected final def convertImpl(
              parentRaiseHandler: RaiseHandler[S, A],
              getState: () => S,
          ): AppliedWidget[NonEmptyList[T]] = {
            val children: NonEmptyList[AppliedWidget[T]] =
              t.map(_.captureReRender.convert(parentRaiseHandler, getState))

            new AppliedWidget[NonEmptyList[T]] {

              override final val valueImpl: IO[NonEmptyList[T]] = children.map(_.value).traverse
              override final val currentImpl: IO[Maybe[ElementT]] =
                children
                  .map(_.current)
                  .traverse
                  .map {
                    _.traverse
                      .map(_.flatten)
                  }
              override final val getElementsAndUpdateImpl: IO[ElementT] =
                children.map(_.getElementsAndUpdate).traverse.map(_.flatten)
            }
          }
        }

    }

}

trait AppliedWidget[+V] {

  private[pye] var widgetName: String = _
  protected val valueImpl: IO[V]
  protected val currentImpl: IO[Maybe[Widget.ElementT]]
  protected val getElementsAndUpdateImpl: IO[Widget.ElementT]

  final val value: IO[V] =
    for {
      _ <- PyeLogger.log.debug(s"value[$widgetName]", "value")
      v <- valueImpl
    } yield v

  final val current: IO[Maybe[Widget.ElementT]] =
    for {
      _ <- PyeLogger.log.debug(s"current[$widgetName]", "current")
      c <- currentImpl
    } yield c

  final val getElementsAndUpdate: IO[Widget.ElementT] =
    for {
      _ <- PyeLogger.log.debug(s"getElementsAndUpdate[$widgetName]", "getElementsAndUpdate")
      e <- getElementsAndUpdateImpl
    } yield e

  private[pye] final val reRender: IO[Widget.ElementT] =
    for {
      // TODO (KR) : There is a bug here...
      //           : Somehow adding this blank `IO {}` fixes it
      //           : (there are 2 of these)
      _ <- IO {}

      mOldElements <- current
      /*
      _ <- IO {
        console.log("--- oldElements ---")
        mOldElements.toList.flatMap(_.toList).foreach(console.log(_))
      }
       */
      newElements <- getElementsAndUpdate
      /*
      _ <- IO {
        console.log("--- newElements ---")
        newElements.foreach(console.log(_))
      }
       */
      _ <- mOldElements.map(Widget.replaceNodes(_, newElements)).traverse
    } yield newElements

}
