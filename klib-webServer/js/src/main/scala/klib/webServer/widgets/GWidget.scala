package klib.webServer.widgets

import klib.fp.types._
import org.scalajs.dom.raw.HTMLElement

sealed abstract class GWidget[Value, S] private (val state: S) {
  protected val stateToValue: S => ?[Value]
  protected val stateToNode: S => HTMLElement

  private var _node: HTMLElement = stateToNode(state)

  def node: HTMLElement = _node
  def value: ?[Value] = stateToValue(state)
  def reRender(): Unit = {
    val newNode = stateToNode(state)
    _node.parentNode.replaceChild(newNode, _node)
    _node = newNode
  }
}

object GWidget {

  def apply[Value, S](_stateToValue: S => ?[Value])(_stateToNode: S => HTMLElement): GWidgetF[Value, S] = { s =>
    new GWidget[Value, S](s) {
      override protected val stateToValue: S => ?[Value] = _stateToValue
      override protected val stateToNode: S => HTMLElement = _stateToNode
    }
  }

}
