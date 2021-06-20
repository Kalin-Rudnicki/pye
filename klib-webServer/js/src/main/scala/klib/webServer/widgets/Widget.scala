package klib.webServer.widgets

import klib.fp.types._
import scalatags.JsDom.Frag

final class Widget[Value, S] private (
    val state: S,
    val node: Frag,
    _value: => ?[Value],
) {
  def value: ?[Value] = _value
}

object Widget {

  final class Builder[Value, S](
      val stateToValue: S => ?[Value],
      val stateToNode: S => Frag,
  ) {
    def apply(s: S): Widget[Value, S] =
      new Widget[Value, S](
        state = s,
        node = stateToNode(s),
        _value = stateToValue(s),
      )
  }

  def apply[Value, S](stateToValue: S => ?[Value])(stateToNode: S => Frag): Builder[Value, S] =
    new Builder[Value, S](
      stateToValue = stateToValue,
      stateToNode = stateToNode,
    )

}
