package klib.webServer.widget

import monocle.Lens
import org.scalajs.dom._

import klib.Implicits._
import klib.fp.types._
import klib.utils.Var

final class RaiseHandler[S, -A](
    private[webServer] val initialState: S,
    private[webServer] val handleRaise: Raise[S, A] => Unit,
) {

  type RaiseT[A2] = Raise[S, A2]

  private[webServer] var _state: S = initialState

  // =====|  |=====

  private def handleRaises[A2 <: A](raises: List[RaiseT[A2]]): Unit =
    raises.foreach(handleRaise)

  def apply[A2 <: A](r0: RaiseT[A2], rN: RaiseT[A2]*): Unit =
    handleRaises(r0 :: rN.toList)

  def raise[A2 <: A](r0: RaiseT[A2], rN: RaiseT[A2]*): Unit =
    handleRaises(r0 :: rN.toList)
  def raises[A2 <: A](rs: List[RaiseT[A2]]): Unit =
    handleRaises(rs)

  def raiseAction[A2 <: A](a0: A2, aN: A2*): Unit =
    handleRaises((a0 :: aN.toList).map(Raise.Action(_)))
  def raiseActions[A2 <: A](as: List[A2]): Unit =
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

  private[webServer] def captureUpdateState(
      widget: Widget[_, S, A],
      elements: Var[Widget.ElementT],
  )(
      withNewState: S => Unit = { _ => },
  ): RaiseHandler[S, A] = {
    val outer = this

    new RaiseHandler[S, A](
      initialState = outer.initialState,
      handleRaise = {
        case updateState: Raise.UpdateState[S] =>
          outer.handleRaise(Raise.UpdateState[S](updateState.updateState, false))
          this._state = updateState.updateState(this._state)
          if (updateState.reRender) {
            console.log("")
            console.log(s"reRender:")
            console.log(_state.toString)
            _state = updateState.updateState(_state)
            console.log(_state.toString)

            val newElements = widget.elementF(this, this._state)
            RaiseHandler.replaceNodes(elements.value, newElements)
            (elements.value = newElements).runSyncOrDump(None)
          }
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

  def watchForUpdates[S, A](
      raiseHandler: RaiseHandler[S, A],
      widget: Widget[_, S, A],
      elements: Var[Widget.ElementT],
  ): Unit =
    ???

}
