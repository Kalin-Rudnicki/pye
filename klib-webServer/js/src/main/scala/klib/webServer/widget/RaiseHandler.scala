package klib.webServer.widget

import monocle.Lens
import org.scalajs.dom._

import klib.Implicits._
import klib.fp.types._

trait RaiseHandler[S, A] {
  type RaiseT = Raise[S, A]

  private[webServer] val initialState: S
  protected var _state: S = initialState

  private[webServer] val handleRaise: RaiseT => Unit

  private var widget: Maybe[Widget[_, S, A]] = None

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

  private[webServer] def handleUpdateAndPass: RaiseHandler[S, A] = {
    val outer = this

    new RaiseHandler[S, A] {
      override private[webServer] val initialState: S = outer.initialState
      override private[webServer] val handleRaise: RaiseT => Unit = {
        case updateState: Raise.UpdateState[S] =>
          console.log(s"updateState(${updateState.reRender}):")
          console.log(_state.toString)
          _state = updateState.updateState(_state)
          console.log(_state.toString)
        case raise =>
          outer.handleRaise(raise)
      }
    }
  }

  private[webServer] def setWidget(widget: Widget[_, S, A]): Unit =
    this.widget match {
      case Some(_) =>
        throw new RuntimeException("Widget is already set...")
      case None =>
        this.widget = widget.some
    }

  private[webServer] def render(): Widget.ElementT =
    this.widget match {
      case Some(widget) =>
        val elements = widget.elementF(this, _state)

        // TODO (KR) :

        elements
      case None =>
        throw new RuntimeException("Widget is not set...")
    }

}

object RaiseHandler {

  def apply[S, A](
      initialState: S,
      handleRaise: Raise[S, A] => Unit,
  ): RaiseHandler[S, A] = {
    val _initialState = initialState
    val _handleRaise = handleRaise

    new RaiseHandler[S, A] {
      override private[webServer] val initialState: S = _initialState
      override private[webServer] val handleRaise: Raise[S, A] => Unit = _handleRaise
    }
  }

}