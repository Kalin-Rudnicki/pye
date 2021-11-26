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

  // =====| Implement |=====

  val widgetName: String

  protected def convertImpl(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V]

  final def convert(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V] = {
    PyeLogger.unsafeLog.debug(s"convert[$widgetName]", "convert")
    val res = convertImpl(parentRaiseHandler, getState)
    res.widgetName = widgetName
    res
  }

  // =====| Combining |=====

  // --- Core ---

  /*
    TODO (KR) : - Should be public?
              : - Should be `apply` not `_apply`?
   */
  private final def _apply[A0 >: A, V2](
      w: Widget[V => V2, S, A0],
  ): Widget[V2, S, A0] =
    WidgetImpls.widgets.apply(this, w)

  final def flatMap[V2, A0 >: A](mapF: V => Widget[V2, S, A0]): Widget[V2, S, A0] =
    WidgetImpls.widgets.flatMap(this, mapF, true)

  final def flatMapBefore[V2, A0 >: A](mapF: V => Widget[V2, S, A0]): Widget[V2, S, A0] =
    WidgetImpls.widgets.flatMap(this, mapF, false)

  // --- Helpers ---

  // =====| Mapping |=====

  // --- Value ---

  final def map[V2](mapF: V => V2): Widget[V2, S, A] = mapValue(mapF)

  final def mapValue_?[V2](mapF: ?[V] => ?[V2]): Widget[V2, S, A] =
    WidgetImpls.widgets.mapV[V, V2, S, A](this, (_, v) => mapF(v))

  final def mapValueWithState[V2](mapF: (S, V) => V2): Widget[V2, S, A] =
    WidgetImpls.widgets.mapV[V, V2, S, A](this, (s, v) => v.map(mapF(s, _)))

  final def mapValue[V2](mapF: V => V2): Widget[V2, S, A] =
    mapValue_?(_.map(mapF))

  final def flatMapValue[V2](mapF: V => ?[V2]): Widget[V2, S, A] =
    mapValue_?(_.flatMap(mapF))

  final def asUnit: Widget[Unit, S, A] =
    mapValue { _ => }

  // --- State ---

  final def zoomOut[S2](s2Lens: Lens[S2, S]): Widget[V, S2, A] =
    WidgetImpls.widgets.zoomOut(this, s2Lens)

  // --- Action ---

  final def covariantMapRaise[A0 >: A, A2](
      mapF: (S, ?[V], Raise[S, A0]) => AsyncIO[List[Raise[S, A2]]],
  ): Widget[V, S, A2] =
    WidgetImpls.widgets.mapA(this, mapF)

  final def covariantMapAction[A0 >: A, A2](
      mapF: (S, ?[V], A0) => AsyncIO[List[Raise[S, A2]]],
  ): Widget[V, S, A2] =
    covariantMapRaise[A0, A2] { (s, v, r) =>
      r match {
        case sou: Raise.StandardOrUpdate[S] =>
          List(sou).pure[AsyncIO]
        case action: Raise.Action[A0] =>
          mapF(s, v, action.action)
      }
    }

  final def ignoreActions: Widget[V, S, Nothing] =
    covariantMapAction[A, Nothing] { (_, _, a) =>
      for {
        _ <- PyeLogger.log.debug(s"Widget ignored action : $a").toAsyncIO
      } yield Nil
    }

  // =====| Wrapping |=====

  final def wrapped(inElement: Modifier => Widget.ElemT): Widget[V, S, A] =
    rsWrapped[A](_ => _ => inElement)
  final def wrappedElems(inElements: Modifier => Widget.ElementT): Widget[V, S, A] =
    rsWrappedElems[A](_ => _ => inElements)

  final def rWrapped[A0 >: A](inElement: RaiseHandler[S, A0] => Modifier => Widget.ElemT): Widget[V, S, A0] =
    rsWrapped[A0](rh => _ => inElement(rh))
  final def rWrappedElems[A0 >: A](inElement: RaiseHandler[S, A0] => Modifier => Widget.ElementT): Widget[V, S, A0] =
    rsWrappedElems[A0](rh => _ => inElement(rh))

  final def sWrapped(inElement: S => Modifier => Widget.ElemT): Widget[V, S, A] =
    rsWrapped[A](_ => inElement)
  final def sWrappedElems(inElement: S => Modifier => Widget.ElementT): Widget[V, S, A] =
    rsWrappedElems[A](_ => inElement)

  final def rsWrapped[A0 >: A](inElement: RaiseHandler[S, A0] => S => Modifier => Widget.ElemT): Widget[V, S, A0] =
    rsWrappedElems[A0](rh => s => elems => NonEmptyList.nel(inElement(rh)(s)(elems)))
  final def rsWrappedElems[A0 >: A](inElements: RaiseHandler[S, A0] => S => Modifier => Widget.ElementT): Widget[V, S, A0] =
    WidgetImpls.widgets.wrapped(this, inElements)

  // =====| ReRender |=====

  final def captureReRender: Widget[V, S, A] = captureReRender(RaiseHandler.ReRender.Nothing)
  final def captureReRender(reRenders: RaiseHandler.ReRender): Widget[V, S, A] =
    WidgetImpls.widgets.captureReRender(this, reRenders)

  final def captureTags(tag0: String, tagN: String*): Widget[V, S, A] =
    WidgetImpls.widgets.captureTags(this, (tag0 :: tagN.toList).toSet)

  // =====| Misc |=====

  final def logRaises(label: String): Widget[V, S, A] =
    WidgetImpls.widgets.logRaises(this, label)

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

  // ---  ---

  override final def toString: String = widgetName

}

// =====| Companion Object |=====

object Widget {

  // =====| Types |=====

  type ElementT = NonEmptyList[Element]
  type ElemT = Element

  type Submit[V, S] = Widget[V, S, CommonRaise.Submit.type]
  type SubmitOr[V, S, O] = Widget[V, S, CommonRaise.SubmitOr[O]]
  type NoAction[V, S] = Widget[V, S, Nothing]

  type Projection[S, A] = { type P[V] = Widget[V, S, A] }
  type ProjectionA[S] = Projection[S, Nothing]

  // =====| Builder |=====

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

    def withValue[V](valueF: S => ?[V]): Widget[V, S, A] =
      WidgetImpls.widgets.leaf(
        elementF,
        valueF,
      )

    def noValue: Widget[Unit, S, A] = withValue[Unit](_ => ().pure[?])

  }

  // =====| TypeClass |=====

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
        Widget.builder.withState[S].withAction[A].element(placeholderSpan).withValue(_ => a.pure[?])

      override def flatMap[V1, V2](t: Widget[V1, S, A], f: V1 => Widget[V2, S, A]): Widget[V2, S, A] =
        t.flatMap(f)

    }

  // TODO (KR) : I question whether this should exist...
  implicit def widgetTraverseList[S, A]: Traverse[List, Widget.Projection[S, A]#P] =
    new Traverse[List, Widget.Projection[S, A]#P] {
      override def traverse[T](t: List[Widget[T, S, A]]): Widget[List[T], S, A] =
        WidgetImpls.widgets.traverseList(t)
    }

  // TODO (KR) : I question whether this should exist...
  implicit def widgetTraverseNonEmptyList[S, A]: Traverse[NonEmptyList, Widget.Projection[S, A]#P] =
    new Traverse[NonEmptyList, Widget.Projection[S, A]#P] {
      override def traverse[T](t: NonEmptyList[Widget[T, S, A]]): Widget[NonEmptyList[T], S, A] =
        WidgetImpls.widgets.traverseNonEmptyList(t)
    }

  // =====| Misc |=====

  def placeholderSpan: Widget.ElemT = span(id := UUID.randomUUID.toString, display.none).render

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
