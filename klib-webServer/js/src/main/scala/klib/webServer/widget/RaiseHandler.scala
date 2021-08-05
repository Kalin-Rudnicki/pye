package klib.webServer.widget

import scala.concurrent.ExecutionContext.Implicits.global

import monocle.Lens
import org.scalajs.dom._

import klib.Implicits._
import klib.fp.types._
import klib.utils.Var
import klib.webServer._

final class RaiseHandler[S, -A](
    private[webServer] val initialState: S,
    private[webServer] val handleRaise: Raise[S, A] => AsyncIO[Unit],
) {

  type RaiseT[A2] = Raise[S, A2]

  private[webServer] var _state: S = initialState
  private[webServer] var _global: Boolean = false

  // =====|  |=====

  private[webServer] def handleRaises[A2 <: A](raises: List[RaiseT[A2]]): AsyncIO[Unit] =
    AsyncIO.runSequentially(raises.map(handleRaise)).map { _ => }

  private def handleAndRun[A2 <: A](raises: List[RaiseT[A2]]): Unit =
    handleRaises(raises).runASync {
      case Alive(_) =>
      case Dead(errors) =>
        errors.map(Raise.DisplayMessage.fromThrowable).foreach(displayMessage)
    }

  def apply[A2 <: A](r0: RaiseT[A2], rN: RaiseT[A2]*): Unit =
    handleAndRun(r0 :: rN.toList)

  def raise[A2 <: A](r0: RaiseT[A2], rN: RaiseT[A2]*): Unit =
    handleAndRun(r0 :: rN.toList)
  def raises[A2 <: A](rs: List[RaiseT[A2]]): Unit =
    handleAndRun(rs)

  def raiseAction[A2 <: A](a0: A2, aN: A2*): Unit =
    handleAndRun((a0 :: aN.toList).map(Raise.Action(_)))
  def raiseActions[A2 <: A](as: List[A2]): Unit =
    handleAndRun(as.map(Raise.Action(_)))

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

    lazy val newRH: RaiseHandler[S, A] =
      new RaiseHandler[S, A](
        initialState = outer.initialState,
        handleRaise = {
          case updateState: Raise.UpdateState[S] =>
            for {
              _ <- outer.handleRaise(Raise.UpdateState[S](updateState.updateState, false))
              _ <- AsyncIO {
                newRH._state = updateState.updateState(newRH._state)
                if (updateState.reRender) {
                  /*
                  console.log("")
                  console.log(s"reRender:")
                  console.log(newRH._state.toString)
                   */

                  val newElements = widget.elementF(newRH, newRH._state)
                  RaiseHandler.replaceNodes(elements.value, newElements)
                  (elements.value = newElements).runSyncOrDump(None)
                }
                withNewState(newRH._state)
              }
            } yield ()
          case raise =>
            outer.handleRaise(raise)
        },
      )

    newRH
  }

}

object RaiseHandler {

  def apply[S, A](
      initialState: S,
      handleRaise: Raise[S, A] => AsyncIO[Unit],
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
    oldElems.foreach { parent.removeChild }

    // console.log("Adding:")
    newElems.foreach { addNode }
  }

  def globalRaiseHandler[S, A](
      initialState: S,
      handleAction: A => AsyncIO[List[Raise.Standard[S]]],
  ): RaiseHandler[S, A] =
    new RaiseHandler[S, A](
      initialState = initialState,
      handleRaise = { raise =>
        def handleStandard(std: Raise.Standard[S]): AsyncIO[Unit] = {
          console.log(std.toString)
          std match {
            case Raise.UpdateState(_, _) =>
              // NOTE : All updates should have been properly handled already...
              AsyncIO {}
            case message: Raise.DisplayMessage =>
              AsyncIO(displayMessage(message))
            case history: Raise.History =>
              // TODO (KR) : Possibly keep track of if the page changed?
              history match {
                case Raise.History.Push(page)    => page._push()
                case Raise.History.Replace(page) => page._replace()
                case Raise.History.Go(delta)     => AsyncIO { window.history.go(delta) }
              }
          }
        }

        def handle(raise: Raise[S, A]): AsyncIO[Unit] =
          raise match {
            case standard: Raise.Standard[S] => handleStandard(standard)
            case Raise.Action(action) =>
              for {
                standards <- handleAction(action)
                _ <- AsyncIO.runSequentially(standards.map(handleStandard))
              } yield ()
          }

        handle(raise)
      },
    )

}
