package pye.widgets

import scala.annotation.tailrec

import monocle.Optional
import monocle.macros.GenLens
import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import pye._
import pye.Implicits._
import pye.Widget.ElementT

trait misc {

  final case class SumTypeSub[V, OS, IS, A](
      lens: Optional[OS, IS],
      iWidget: Widget[V, IS, A],
  ) {

    def oaWidget(
        getState: () => OS,
        parentRH: RaiseHandler[OS, A],
    ): (OS => Boolean, AppliedWidget[V]) =
      (
        lens.getOption(_).nonEmpty, {
          val rh: RaiseHandler[IS, A] = ??? // TODO (KR) :
          val unsafeGetter: () => IS = () => lens.getOption(getState()).get
          Widget.simpleRhCaptureReRender(iWidget, unsafeGetter, rh)
        },
      )

  }

  // REMOVE : ...
  /*
  def noneOptional[T]: Optional[Maybe[T], None.type] =
    Optional[Maybe[T], None.type] {
      case none @ None => none.someOpt
      case Some(_)     => scala.None
    } { n => _ => n }
   */

  def fromSumTypes[V, S, A](
      sumName: String,
      subs: SumTypeSub[V, S, _, A]*,
  ): Widget[V, S, A] =
    new Widget[V, S, A] {
      override final val widgetName: String = sumName
      override final protected def convertImpl(
          parentRaiseHandler: RaiseHandler[S, A],
          getState: () => S,
      ): AppliedWidget[V] =
        new AppliedWidget[V] {
          private final val inner: Var[Maybe[AppliedWidget[V]]] = Var(None)
          private final val aSubs: List[(S => Boolean, AppliedWidget[V])] =
            subs.toList.map(_.oaWidget(getState, parentRaiseHandler))
          @tailrec
          private final def firstSubMatch(
              s: S,
              remSubs: List[(S => Boolean, AppliedWidget[V])] = aSubs,
          ): ?[AppliedWidget[V]] =
            remSubs match {
              case (valid, widget) :: tail =>
                if (valid(s)) widget.pure[?] else firstSubMatch(s, tail)
              case Nil =>
                ?.dead(Message(s"Non-Exhaustive match for Widget $sumName"))
            }

          override protected final val valueImpl: IO[V] =
            for {
              i1 <- IO { inner.value }
              i2 <- IO.wrapEffect { i1.toEA(Message("No inner")) }
              v <- i2.value
            } yield v
          override protected final val currentImpl: IO[Maybe[ElementT]] =
            for {
              i1 <- IO { inner.value }
              e <- i1.map(_.current).traverse.map(_.flatten)
            } yield e
          override protected final val getElementsAndUpdateImpl: IO[ElementT] =
            for {
              s <- IO { getState() }
              newInner <- IO.wrapEffect { firstSubMatch(s) }
              e <- newInner.getElementsAndUpdate
            } yield e
        }
    }

  def maybeW[V, S, A](widget: Widget[V, S, A]): Widget[Maybe[V], Maybe[S], A] =
    fromSumTypes[Maybe[V], Maybe[S], A](
      "Maybe",
      SumTypeSub[Maybe[V], Maybe[S], Some[S], A](
        Optional[Maybe[S], Some[S]] {
          case some @ Some(_) => some.someOpt
          case None           => scala.None
        } { st => _ => st },
        widget.zoomOut(GenLens[Some[S]](_.a)).mapValue(_.some),
      ),
      SumTypeSub[Maybe[V], Maybe[S], None.type, A](
        Optional[Maybe[S], None.type] {
          case None    => None.someOpt
          case Some(_) => scala.None
        } { st => _ => st },
        Widget.builder
          .withState[None.type]
          .withAction[A]
          .element(Widget.placeholderSpan)
          .withValue(_.pure[?]),
      ),
    )

  def formModalW[V, S, A](
      widget: Widget[V, S, CommonRaise.Submit.type],
      onSubmit: V => AsyncIO[List[Raise[S, A]]],
      modalModifier: Modifier = Seq.empty[Modifier],
      containerModifier: Modifier = Seq.empty[Modifier],
  ): Widget[Unit, Maybe[S], A] =
    new Widget[Unit, Maybe[S], A] {
      override final val widgetName: String = "FormModal"
      override final protected def convertImpl(
          parentRaiseHandler: RaiseHandler[Maybe[S], A],
          getState: () => Maybe[S],
      ): AppliedWidget[Unit] =
        new AppliedWidget[Unit] {
          private val child: Var[Maybe[AppliedWidget[V]]] = Var(None)
          private val elems: Var[Maybe[Widget.ElementT]] = Var(None)

          override protected val valueImpl: IO[Unit] = ().pure[IO]
          override protected val currentImpl: IO[Maybe[ElementT]] = IO { elems.value }
          override protected val getElementsAndUpdateImpl: IO[ElementT] = ???
        }
    }

}
object misc extends misc
