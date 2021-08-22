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
import pye.widgets.modifiers._

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
          val rh: RaiseHandler[IS, A] = {
            case sou: Raise.StandardOrUpdate[IS] =>
              sou match {
                case update: Raise.UpdateState[IS] =>
                  parentRH._handleRaise(Raise.UpdateState[OS](lens.modify(update.update), update.reRender))
                case standard: Raise.Standard =>
                  parentRH._handleRaise(standard)
              }
            case action: Raise.Action[A] =>
              parentRH._handleRaise(action)
          }
          val unsafeGetter: () => IS = () => lens.getOption(getState()).get
          iWidget.captureReRender.convert(rh, unsafeGetter)
        },
      )

  }

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
          override protected final val currentImpl: IO[Maybe[Widget.ElementT]] =
            for {
              i1 <- IO { inner.value }
              e <- i1.map(_.current).traverse.map(_.flatten)
            } yield e
          override protected final val getElementsAndUpdateImpl: IO[Widget.ElementT] =
            for {
              s <- IO { getState() }
              newInner <- IO.wrapEffect { firstSubMatch(s) }
              _ <- inner.value = newInner.some
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

  sealed trait ModalAction[+A]
  object ModalAction {
    case object Close extends ModalAction[Nothing]
    final case class Action[A](raise: A) extends ModalAction[A]
  }

  def modalW[V, S, A](
      widget: Widget[V, S, A],
      marginT: String,
      marginLR: String,
      marginB: Maybe[String] = None,
      z: Int = 1,
      modalDecorator: Modifier = Seq.empty[Modifier],
      containerDecorator: Modifier = Seq.empty[Modifier],
  ): Widget[Maybe[V], Maybe[S], A] =
    maybeW {
      widget
        .mapAction[A, ModalAction[A]] { (_, _, a) => AsyncIO { List(Raise.Action(ModalAction.Action(a))) } }
        .rWrapped[ModalAction[A]] { rh => elems =>
          div(
            PyeS.`pye:modal`,
            display.block,
            position.fixed,
            left := 0,
            top := 0,
            width := "100vw",
            height := "100vh",
            zIndex := z,
            backgroundColor := "rgba(0, 0, 0, 0.75)",
            onclick := { (_: Event) =>
              rh.raiseAction(ModalAction.Close)
            },
          )(
            div(
              PyeS.`pye:modal`.container,
              width := s"calc(100vw - 2 * ($marginLR))",
              height := {
                marginB match {
                  case Some(marginB) => s"calc(100vh - (($marginT) + ($marginB)))"
                  case None          => s"calc(100vh - 2 * ($marginT))"
                }
              },
              marginTop := marginT,
              marginBottom := marginB.getOrElse(marginT),
              marginLeft := marginLR,
              marginRight := marginLR,
              onclick := { (e: Event) =>
                e.stopPropagation()
              },
            )(
              elems,
            )(containerDecorator),
          )(modalDecorator).render
        }
    }.mapAction[ModalAction[A], A] { (_, _, a) =>
      AsyncIO {
        List(
          a match {
            case ModalAction.Close         => Raise.UpdateState[Maybe[S]](_ => None)
            case ModalAction.Action(raise) => Raise.Action(raise)
          },
        )
      }
    }

}
object misc extends misc
