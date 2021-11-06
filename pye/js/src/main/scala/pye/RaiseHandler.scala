package pye

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.types._
import pye.Implicits._

trait RaiseHandler[S, -A] {

  // TODO (KR) : def _handleRaise(raise, childrenThatRequestedReRender)
  //           : - then, only reRender at the end
  //           : - this should be more efficient than possibly reRendering sub-trees multiple times

  protected def _handleRaiseImpl(raise: Raise[S, A]): AsyncIO[RaiseHandler.ReRender]

  private[pye] def _handleRaise(raise: Raise[S, A]): AsyncIO[RaiseHandler.ReRender] =
    for {
      _ <- PyeLogger.log.debug(raise.toString, "raise").toAsyncIO
      prr <- _handleRaiseImpl(raise)
    } yield prr
  private[pye] def _handleRaises(raises: List[Raise[S, A]]): AsyncIO[RaiseHandler.ReRender] =
    for {
      prrs <- AsyncIO.runSequentially(raises.map(_handleRaise))
    } yield RaiseHandler.ReRender.merge(prrs)

  def raise(r0: Raise[S, A], rN: Raise[S, A]*): Unit =
    raises(r0 :: rN.toList)
  def raises(raises: List[Raise[S, A]]): Unit = {
    for {
      rr <- _handleRaises(raises)
      _ <- PyeLogger.log.debug(s"start reRender: $rr", "reRender").toAsyncIO
      _ <- rr match {
        case RaiseHandler.ReRender.Nothing    => AsyncIO {}
        case RaiseHandler.ReRender.PageChange => AsyncIO {}
        case RaiseHandler.ReRender.Widgets(widgets) =>
          widgets.toList
            .map { w =>
              for {
                _ <- PyeLogger.log.debug(s"reRender: $w", "reRender")
                _ <- w.reRender
              } yield ()
            }
            .traverse
            .toAsyncIO
      }
      _ <- PyeLogger.log.debug(s"end reRender", "reRender").toAsyncIO
    } yield ()
  }.runAndShowErrors()

  def raiseAction(a0: A, aN: A*): Unit =
    raiseActions(a0 :: aN.toList)
  def raiseActions(actions: List[A]): Unit =
    raises(actions.map(Raise.Action(_)))

  def state: RaiseHandler.State[S] = new RaiseHandler.State(this)

  def history: RaiseHandler.History = new RaiseHandler.History(this)

}

object RaiseHandler {

  sealed trait ReRender {

    def orChild(child: ReRender): ReRender =
      this match {
        case ReRender.Nothing    => child
        case ReRender.PageChange => this
        case ReRender.Widgets(_) =>
          child match {
            case ReRender.Nothing    => this
            case ReRender.PageChange => child
            case ReRender.Widgets(_) => this
          }
      }

  }
  object ReRender {

    case object Nothing extends ReRender
    case object PageChange extends ReRender
    final case class Widgets(widgets: NonEmptyList[AppliedWidget[_]]) extends ReRender

    def apply(widgets: AppliedWidget[_]*): ReRender =
      widgets.toList.toNel match {
        case Some(widgets) => Widgets(widgets)
        case None          => Nothing
      }

    def merge(rrs: List[ReRender]): ReRender = {
      @tailrec
      def loop(
          queue: List[ReRender],
          widgets: Maybe[NonEmptyList[AppliedWidget[_]]],
      ): ReRender =
        queue match {
          case head :: tail =>
            head match {
              case Nothing    => loop(tail, widgets)
              case PageChange => PageChange
              case Widgets(w) => loop(tail, widgets.cata(w ::: _, w).some)
            }
          case Nil =>
            widgets match {
              case Some(widgets) => Widgets(widgets)
              case None          => Nothing
            }
        }

      loop(rrs, None)
    }

  }

  final class State[S](rh: RaiseHandler[S, _]) {

    def update(update: S => S, reRender: Boolean = true): Unit =
      rh.raise(Raise.UpdateState[S](update, reRender))

    def set(newState: S, reRender: Boolean = true): Unit =
      update(_ => newState, reRender)

  }
  final class History(rh: RaiseHandler[_, _]) {

    def push(page: => Page): Unit =
      rh.raise(Raise.History.push(page))

    def replace(page: => Page): Unit =
      rh.raise(Raise.History.replace(page))

  }

}
