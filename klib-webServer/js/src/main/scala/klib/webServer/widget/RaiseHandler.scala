package klib.webServer.widget

import monocle.Lens
import org.scalajs.dom._

import klib.Implicits._
import klib.fp.types._

final class RaiseHandler[S, A](
    private[webServer] val initialState: S,
    private[webServer] val handleRaise: Raise[S, A] => Unit,
) {

  type RaiseT = Raise[S, A]

  private[webServer] var _state: S = initialState

  // =====|  |=====

  // TODO (KR) :
  private def handleRaises(raises: List[RaiseT]): Unit =
    raises.foreach(handleRaise)

  def apply(r0: RaiseT, rN: RaiseT*): Unit =
    handleRaises(r0 :: rN.toList)

  def raise(r0: RaiseT, rN: RaiseT*): Unit =
    handleRaises(r0 :: rN.toList)
  def raises(rs: List[RaiseT]): Unit =
    handleRaises(rs)

  def raiseAction(a0: A, aN: A*): Unit =
    handleRaises((a0 :: aN.toList).map(Raise.Action(_)))
  def raiseActions(as: List[A]): Unit =
    handleRaises(as.map(Raise.Action(_)))

  // =====|  |=====

  def zoomIn[S2](lens: Lens[S, S2]): RaiseHandler[S2, A] =
    RaiseHandler[S2, A](
      initialState = lens.get(initialState),
      handleRaise = {
        case standard: Raise.Standard[S2] =>
          standard match {
            case updateState: Raise.UpdateState[S2] =>
              handleRaise(Raise.UpdateState[S](lens.modify(updateState.updateState), updateState.reRender))
            case displayMessage: Raise.DisplayMessage =>
              handleRaise(displayMessage)
            case history: Raise.History =>
              handleRaise(history)
          }
        case action: Raise.Action[A] =>
          handleRaise(action)
      },
    )

  private[webServer] def captureUpdateState(withNewState: S => Unit = { _ => }): RaiseHandler[S, A] = {
    val outer = this

    new RaiseHandler[S, A](
      initialState = outer.initialState,
      handleRaise = {
        case updateState: Raise.UpdateState[S] =>
          console.log("")
          console.log(s"updateState(${updateState.reRender}):")
          console.log(_state.toString)
          _state = updateState.updateState(_state)
          console.log(_state.toString)
          outer.handleRaise(Raise.UpdateState[S](updateState.updateState, false))
          withNewState(_state)
        case raise =>
          outer.handleRaise(raise)
      },
    )
  }

}

object RaiseHandler {

  def apply[S, A](
      initialState: S,
      handleRaise: Raise[S, A] => Unit,
  ): RaiseHandler[S, A] =
    new RaiseHandler[S, A](
      initialState = initialState,
      handleRaise = handleRaise,
    )

  def replaceNodes(oldElems: Widget.ElementT, newElems: Widget.ElementT): Unit = {
    val parent = oldElems.head.parentNode
    val addNode: Node => Unit =
      Maybe(oldElems.toList.last.nextSibling) match {
        case Some(nextSibling) =>
          parent.insertBefore(_, nextSibling)
        case None =>
          parent.appendChild
      }
    oldElems.foreach(parent.removeChild)
    newElems.foreach(addNode)
  }

}
