package klib.webServer.widgets

import monocle.Lens
import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.fp.types._

trait Widget2[S, A] {

  def element(raiseHandler: Widget2.RaiseHandler[S, A], state: S): Element

  def render(handleAction: A => Unit): Element = {

    // TODO (KR) :
    ???
  }

  def zoomOut[S2](lens: Lens[S2, S]): Widget2[S2, A] = { (rh2, s2) =>
    element(rh2.zoomIn(lens), lens.get(s2))
  }

}

object Widget2 {

  trait RaiseHandler[S, A] {

    def handle(raise: Raise[S, A]): Unit

    def zoomIn[S2](lens: Lens[S, S2]): RaiseHandler[S2, A] = {
      case Raise.UpdateState(updateState: (S2 => S2)) =>
        handle(
          Raise.UpdateState[S](lens.modify(updateState)),
        )
      case action @ Raise.Action(_) =>
        handle(action)
    }

  }

  sealed trait Raise[+S, +A]
  object Raise {
    final case class UpdateState[S](updateState: S => S) extends Raise[S, Nothing]
    final case class Action[+A](action: A) extends Raise[Nothing, A]
  }

}
