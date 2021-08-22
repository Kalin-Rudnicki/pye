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
      _ <- rr match {
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
    } yield ()
  }.runAndShowErrors()

  def raiseAction(a0: A, aN: A*): Unit =
    raiseActions(a0 :: aN.toList)
  def raiseActions(actions: List[A]): Unit =
    raises(actions.map(Raise.Action(_)))

  def updateState(update: S => S, reRender: Boolean = true): Unit =
    raise(Raise.UpdateState[S](update, reRender))

  def setState(newState: S, reRender: Boolean = true): Unit =
    updateState(_ => newState, reRender)

}

object RaiseHandler {

  sealed trait ReRender {

    def orChild(other: ReRender): ReRender =
      this match {
        case ReRender.PageChange => this
        case ReRender.Widgets(_) =>
          other match {
            case ReRender.PageChange => other
            case ReRender.Widgets(_) => this
          }
      }

  }
  object ReRender {

    case object PageChange extends ReRender
    final case class Widgets(widgets: Set[AppliedWidget[_]]) extends ReRender

    def apply(widgets: AppliedWidget[_]*): ReRender =
      Widgets(widgets.toSet)

    def merge(rrs: List[ReRender]): ReRender = {
      @tailrec
      def loop(
          queue: List[ReRender],
          widgets: Set[AppliedWidget[_]],
      ): ReRender =
        queue match {
          case head :: tail =>
            head match {
              case PageChange => PageChange
              case Widgets(w) => loop(tail, w | widgets)
            }
          case Nil =>
            Widgets(widgets)
        }

      loop(rrs, Set.empty)
    }

  }

}
