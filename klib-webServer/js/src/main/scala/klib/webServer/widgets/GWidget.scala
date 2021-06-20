package klib.webServer.widgets

import klib.fp.types._
import org.scalajs.dom.raw.HTMLElement

sealed abstract class GWidget[Value, S] private (val state: S) {
  val stateToValue: S => ?[Value]
  val stateToNode: S => HTMLElement

  private final var _node: HTMLElement = stateToNode(state)

  final def node: HTMLElement = _node
  final def value: ?[Value] = stateToValue(state)
  final def reRender(): Unit = {
    val newNode = stateToNode(state)
    _node.parentNode.replaceChild(newNode, _node)
    _node = newNode
  }
}

object GWidget {

  def apply[Value, S](_stateToValue: S => ?[Value])(_stateToNode: S => HTMLElement): GWidgetF[Value, S] = { s =>
    new GWidget[Value, S](s) {
      override final val stateToValue: S => ?[Value] = _stateToValue
      override final val stateToNode: S => HTMLElement = _stateToNode
    }
  }

}
