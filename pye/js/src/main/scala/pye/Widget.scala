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
import pye.CommonRaise.SubmitOr
import pye.Implicits._
import pye.widgets.modifiers.PyeS

trait Widget[V, S, +A] {

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

  // --- myUpdatesReRenderYou ---

  def myUpdatesReRenderYou[A0 >: A, V2](
      w: Widget[V2, S, A0],
      youAppearAfterMe: Boolean,
  ): Widget[Unit, S, A0] =
    vMyUpdatesReRenderYou[A0, V2, Unit](w, (_, _) => (), youAppearAfterMe)

  def tMyUpdatesReRenderYou[A0 >: A, V2](
      w: Widget[V2, S, A0],
      youAppearAfterMe: Boolean,
  ): Widget[(V, V2), S, A0] =
    vMyUpdatesReRenderYou[A0, V2, (V, V2)](w, (_, _), youAppearAfterMe)

  def vMyUpdatesReRenderYou[A0 >: A, V2, V3](
      w: Widget[V2, S, A0],
      mapV: (V, V2) => V3,
      youAppearAfterMe: Boolean,
  ): Widget[V3, S, A0] =
    WidgetImpls.widgets.joinWithReRenderLogic[V, V2, V3, S, A0](
      w1 = this,
      w2 = w,
      w1ReRendersW2 = true,
      w2ReRendersW1 = false,
      w1BeforeW2 = youAppearAfterMe,
      mapV = mapV,
    )

  def >+>[A0 >: A, V2](w: Widget[V2, S, A0]): Widget[Unit, S, A0] =
    myUpdatesReRenderYou(w, true)

  def >+<[A0 >: A, V2](w: Widget[V2, S, A0]): Widget[Unit, S, A0] =
    myUpdatesReRenderYou(w, false)

  // --- yourUpdatesReRenderMe ---

  def yourUpdatesReRenderMe[A0 >: A, V2](
      w: Widget[V2, S, A0],
      youAppearAfterMe: Boolean,
  ): Widget[Unit, S, A0] =
    vMyUpdatesReRenderYou[A0, V2, Unit](w, (_, _) => (), youAppearAfterMe)

  def tYourUpdatesReRenderMe[A0 >: A, V2](
      w: Widget[V2, S, A0],
      youAppearAfterMe: Boolean,
  ): Widget[(V, V2), S, A0] =
    vYourUpdatesReRenderMe[A0, V2, (V, V2)](w, (_, _), youAppearAfterMe)

  def vYourUpdatesReRenderMe[A0 >: A, V2, V3](
      w: Widget[V2, S, A0],
      mapV: (V, V2) => V3,
      youAppearAfterMe: Boolean,
  ): Widget[V3, S, A0] =
    WidgetImpls.widgets.joinWithReRenderLogic[V, V2, V3, S, A0](
      w1 = this,
      w2 = w,
      w1ReRendersW2 = false,
      w2ReRendersW1 = true,
      w1BeforeW2 = youAppearAfterMe,
      mapV = mapV,
    )

  def <+>[A0 >: A, V2](w: Widget[V2, S, A0]): Widget[Unit, S, A0] =
    yourUpdatesReRenderMe(w, true)

  def <+<[A0 >: A, V2](w: Widget[V2, S, A0]): Widget[Unit, S, A0] =
    yourUpdatesReRenderMe(w, false)

  // --- ourUpdatesReRenderEachOther ---

  def ourUpdatesReRenderEachOther[A0 >: A, V2](
      w: Widget[V2, S, A0],
      youAppearAfterMe: Boolean,
  ): Widget[Unit, S, A0] =
    vOurUpdatesReRenderEachOther[A0, V2, Unit](w, (_, _) => (), youAppearAfterMe)

  def tOurUpdatesReRenderEachOther[A0 >: A, V2](
      w: Widget[V2, S, A0],
      youAppearAfterMe: Boolean,
  ): Widget[(V, V2), S, A0] =
    vOurUpdatesReRenderEachOther[A0, V2, (V, V2)](w, (_, _), youAppearAfterMe)

  def vOurUpdatesReRenderEachOther[A0 >: A, V2, V3](
      w: Widget[V2, S, A0],
      mapV: (V, V2) => V3,
      youAppearAfterMe: Boolean,
  ): Widget[V3, S, A0] =
    WidgetImpls.widgets.joinWithReRenderLogic[V, V2, V3, S, A0](
      w1 = this,
      w2 = w,
      w1ReRendersW2 = true,
      w2ReRendersW1 = true,
      w1BeforeW2 = youAppearAfterMe,
      mapV = mapV,
    )

  def <>+>[A0 >: A, V2](w: Widget[V2, S, A0]): Widget[Unit, S, A0] =
    ourUpdatesReRenderEachOther(w, true)

  def <>+<[A0 >: A, V2](w: Widget[V2, S, A0]): Widget[Unit, S, A0] =
    ourUpdatesReRenderEachOther(w, false)

  // =====| Misc |=====

  final def logRaises(label: String): Widget[V, S, A] =
    WidgetImpls.widgets.logRaises(this, label)

  final def labelled(
      labelText: String,
      decorator: Modifier = Seq.empty[Modifier],
  ): Widget[V, S, A] =
    Widget
      .constrainA[S, A]
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

  // =====| Type Constraints |=====

  def constrain[S]: ado[Widget.Projection[S, Nothing]#P] =
    ado[Widget.Projection[S, Nothing]#P]

  def constrainA[S, A]: ado[Widget.Projection[S, A]#P] =
    ado[Widget.Projection[S, A]#P]

  def constrainSubmit[S]: ado[Widget.Projection[S, CommonRaise.Submit.type]#P] =
    constrainA[S, CommonRaise.Submit.type]

  def constrainSubmitOr[S, O]: ado[Widget.Projection[S, CommonRaise.SubmitOr[O]]#P] =
    constrainA[S, CommonRaise.SubmitOr[O]]

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

  // =====| Implicits |=====

  trait Implicits {

    implicit class InvariantWidgetOps[V, S, A](widget: Widget[V, S, A]) {

      // =====| Mapping Actions |=====

      // --- mapRaise ---

      def mapRaise[A2](mapF: Raise[S, A] => AsyncIO[List[Raise[S, A2]]]): Widget[V, S, A2] =
        widget.covariantMapRaise[A, A2]((_, _, a) => mapF(a))

      def sMapRaise[A2](mapF: (S, Raise[S, A]) => AsyncIO[List[Raise[S, A2]]]): Widget[V, S, A2] =
        widget.covariantMapRaise[A, A2]((s, _, a) => mapF(s, a))

      def vMapRaise[A2](mapF: (V, Raise[S, A]) => AsyncIO[List[Raise[S, A2]]]): Widget[V, S, A2] =
        widget.covariantMapRaise[A, A2] { (_, v, a) =>
          for {
            v <- v.toAsyncIO
            r <- mapF(v, a)
          } yield r
        }

      def svMapRaise[A2](mapF: (S, V, Raise[S, A]) => AsyncIO[List[Raise[S, A2]]]): Widget[V, S, A2] =
        widget.covariantMapRaise[A, A2] { (s, v, a) =>
          for {
            v <- v.toAsyncIO
            r <- mapF(s, v, a)
          } yield r
        }

      def vMapRaise_?[A2](mapF: (?[V], Raise[S, A]) => AsyncIO[List[Raise[S, A2]]]): Widget[V, S, A2] =
        widget.covariantMapRaise[A, A2]((_, v, a) => mapF(v, a))

      def svMapRaise_?[A2](mapF: (S, ?[V], Raise[S, A]) => AsyncIO[List[Raise[S, A2]]]): Widget[V, S, A2] =
        widget.covariantMapRaise[A, A2](mapF)

      // --- handleRaise ---

      def handleRaise(mapF: Raise[S, A] => AsyncIO[List[Raise[S, Nothing]]]): Widget[V, S, Nothing] =
        mapRaise[Nothing](mapF)

      def sHandleRaise(mapF: (S, Raise[S, A]) => AsyncIO[List[Raise[S, Nothing]]]): Widget[V, S, Nothing] =
        sMapRaise[Nothing](mapF)

      def vHandleRaise(mapF: (V, Raise[S, A]) => AsyncIO[List[Raise[S, Nothing]]]): Widget[V, S, Nothing] =
        vMapRaise[Nothing](mapF)

      def svHandleRaise(mapF: (S, V, Raise[S, A]) => AsyncIO[List[Raise[S, Nothing]]]): Widget[V, S, Nothing] =
        svMapRaise[Nothing](mapF)

      def vHandleRaise_?(mapF: (?[V], Raise[S, A]) => AsyncIO[List[Raise[S, Nothing]]]): Widget[V, S, Nothing] =
        vMapRaise_?[Nothing](mapF)

      def svHandleRaise_?(mapF: (S, ?[V], Raise[S, A]) => AsyncIO[List[Raise[S, Nothing]]]): Widget[V, S, Nothing] =
        svMapRaise_?[Nothing](mapF)

      // --- mapAction ---

      def mapAction[A2](mapF: A => AsyncIO[List[Raise[S, A2]]]): Widget[V, S, A2] =
        widget.covariantMapAction[A, A2]((_, _, a) => mapF(a))

      def sMapAction[A2](mapF: (S, A) => AsyncIO[List[Raise[S, A2]]]): Widget[V, S, A2] =
        widget.covariantMapAction[A, A2]((s, _, a) => mapF(s, a))

      def vMapAction[A2](mapF: (V, A) => AsyncIO[List[Raise[S, A2]]]): Widget[V, S, A2] =
        widget.covariantMapAction[A, A2] { (_, v, a) =>
          for {
            v <- v.toAsyncIO
            r <- mapF(v, a)
          } yield r
        }

      def svMapAction[A2](mapF: (S, V, A) => AsyncIO[List[Raise[S, A2]]]): Widget[V, S, A2] =
        widget.covariantMapAction[A, A2] { (s, v, a) =>
          for {
            v <- v.toAsyncIO
            r <- mapF(s, v, a)
          } yield r
        }

      def vMapAction_?[A2](mapF: (?[V], A) => AsyncIO[List[Raise[S, A2]]]): Widget[V, S, A2] =
        widget.covariantMapAction[A, A2]((_, v, a) => mapF(v, a))

      def svMapAction_?[A2](mapF: (S, ?[V], A) => AsyncIO[List[Raise[S, A2]]]): Widget[V, S, A2] =
        widget.covariantMapAction[A, A2](mapF)

      // --- handleAction ---

      def handleAction(mapF: A => AsyncIO[List[Raise[S, Nothing]]]): Widget[V, S, Nothing] =
        mapAction[Nothing](mapF)

      def sHandleAction(mapF: (S, A) => AsyncIO[List[Raise[S, Nothing]]]): Widget[V, S, Nothing] =
        sMapAction[Nothing](mapF)

      def vHandleAction(mapF: (V, A) => AsyncIO[List[Raise[S, Nothing]]]): Widget[V, S, Nothing] =
        vMapAction[Nothing](mapF)

      def svHandleAction(mapF: (S, V, A) => AsyncIO[List[Raise[S, Nothing]]]): Widget[V, S, Nothing] =
        svMapAction[Nothing](mapF)

      def vHandleAction_?(mapF: (?[V], A) => AsyncIO[List[Raise[S, Nothing]]]): Widget[V, S, Nothing] =
        vMapAction_?[Nothing](mapF)

      def svHandleAction_?(mapF: (S, ?[V], A) => AsyncIO[List[Raise[S, Nothing]]]): Widget[V, S, Nothing] =
        svMapAction_?[Nothing](mapF)

    }

    // TODO (KR) : Make builder.
    //           : Scala is having an un-acceptable level type inference here
    implicit class WidgetFormOps[V, S, O](widget: Widget[V, S, CommonRaise.SubmitOr[O]]) {

      def addSubmitButton(
          submitButtonLabel: String = "Submit",
          submitButtonDecorator: Modifier = Seq.empty[Modifier],
      ): Widget[V, S, CommonRaise.SubmitOr[O]] =
        Widget
          .constrainSubmitOr[S, O]
          .join(
            widget,
            br.asWidget,
            widgets.forms.submitButton(submitButtonLabel, submitButtonDecorator),
          )
          .mapValue(_._1)

      def handleSubmit[A >: O](
          onSubmit: AsyncIO[List[Raise[S, A]]],
      ): Widget[V, S, A] =
        widget.mapAction {
          case CommonRaise.Submit          => onSubmit
          case CommonRaise.SubmitOr.Or(or) => AsyncIO { Raise.Action(or) :: Nil }
        }

      def sHandleSubmit[A >: O](
          onSubmit: S => AsyncIO[List[Raise[S, A]]],
      ): Widget[V, S, A] =
        widget.sMapAction { (s, a) =>
          a match {
            case CommonRaise.Submit          => onSubmit(s)
            case CommonRaise.SubmitOr.Or(or) => AsyncIO { Raise.Action(or) :: Nil }
          }
        }

      def vHandleSubmit[A >: O](
          onSubmit: V => AsyncIO[List[Raise[S, A]]],
      ): Widget[V, S, A] =
        widget.vMapAction { (v, a) =>
          a match {
            case CommonRaise.Submit          => onSubmit(v)
            case CommonRaise.SubmitOr.Or(or) => AsyncIO { Raise.Action(or) :: Nil }
          }
        }

      def svHandleSubmit[A >: O](
          onSubmit: (S, V) => AsyncIO[List[Raise[S, A]]],
      ): Widget[V, S, A] =
        widget.svMapAction { (s, v, a) =>
          a match {
            case CommonRaise.Submit          => onSubmit(s, v)
            case CommonRaise.SubmitOr.Or(or) => AsyncIO { Raise.Action(or) :: Nil }
          }
        }

    }

    implicit class KeyedActionWidgetOps[V, S, KA_S, KA_K, KA_A](
        widget: Widget[V, S, widgets.all.KeyedAction[(KA_S, KA_K), KA_A]],
    ) {

      def stripKeyedAction: Widget[V, S, KA_A] =
        widget.covariantMapAction[widgets.all.KeyedAction[(KA_S, KA_K), KA_A], KA_A] { (_, _, a) =>
          AsyncIO { Raise.Action(a.action) :: Nil }
        }

    }

  }
  object Implicits extends Implicits

}

// =====| AppliedWidget |=====

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
