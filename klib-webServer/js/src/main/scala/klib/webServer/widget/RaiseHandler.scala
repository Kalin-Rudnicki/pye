package klib.webServer.widget

import monocle.Lens

final case class RaiseHandler[S, A](
    private[webServer] val handleRaises: List[Raise[S, A]] => Unit,
) {
  type RaiseT = Raise[S, A]

  // =====|  |=====

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
    RaiseHandler[S2, A] { raises =>
      handleRaises {
        raises.map[Raise[S, A]] {
          case standard: Raise.Standard[S2, A] =>
            standard match {
              case updateState: Raise.UpdateState[S2] =>
                Raise.UpdateState[S](lens.modify(updateState.updateState), updateState.force)
              case displayMessage: Raise.DisplayMessage =>
                displayMessage
              case history: Raise.History =>
                history
            }
          case action: Raise.Action[A] =>
            action
        }
      }
    }

}
