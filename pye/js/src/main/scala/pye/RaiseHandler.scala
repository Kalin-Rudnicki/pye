package pye

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import pye.Implicits._

trait RaiseHandler[S, -A] {

  // TODO (KR) : def _handleRaise(raise, childrenThatRequestedReRender)
  //           : - then, only reRender at the end
  //           : - this should be more efficient than possibly reRendering sub-trees multiple times

  protected def _handleRaiseImpl(raise: Raise[S, A]): AsyncIO[Unit]

  private[pye] def _handleRaise(raise: Raise[S, A]): AsyncIO[Unit] =
    for {
      _ <- PyeLogger.log.debug(raise.toString, "raise").toAsyncIO
      _ <- _handleRaise(raise)
    } yield ()
  private[pye] def _handleRaises(raises: List[Raise[S, A]]): AsyncIO[Unit] =
    AsyncIO.runSequentially(raises.map(_handleRaise)).map { _ => }

  def raise(r0: Raise[S, A], rN: Raise[S, A]*): Unit =
    raises(r0 :: rN.toList)
  def raises(raises: List[Raise[S, A]]): Unit =
    _handleRaises(raises).runAndShowErrors()

  def raiseAction(a0: A, aN: A*): Unit =
    raiseActions(a0 :: aN.toList)
  def raiseActions(actions: List[A]): Unit =
    raises(actions.map(Raise.Action(_)))

  def updateState(update: S => S, reRender: Boolean = true): Unit =
    raise(Raise.UpdateState[S](update, reRender))

  def setState(newState: S, reRender: Boolean = true): Unit =
    updateState(_ => newState, reRender)

}
