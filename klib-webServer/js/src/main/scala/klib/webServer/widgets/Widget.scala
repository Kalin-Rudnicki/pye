package klib.webServer.widgets

import klib.Implicits._
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

  final class Builder[Value, S] private[Widget] (
      val stateToValue: S => ?[Value],
      val stateToNode: S => Frag,
  ) {

    def apply(s: S): Widget[Value, S] =
      new Widget[Value, S](
        state = s,
        node = stateToNode(s),
        _value = stateToValue(s),
      )

    def wrap(in: Frag => Frag): Builder[Value, S] =
      new Builder[Value, S](
        stateToValue = stateToValue,
        stateToNode = s => in(stateToNode(s)),
      )

    // ---  ---

    def mapErrors(f: Throwable => Throwable): Builder[Value, S] =
      new Builder[Value, S](
        stateToValue = stateToValue(_).mapErrors(f),
        stateToNode = stateToNode,
      )

    def map[Value2](f: Value => Value2): Builder[Value2, S] =
      new Builder[Value2, S](
        stateToValue = stateToValue(_).map(f),
        stateToNode = stateToNode,
      )

    def flatMap[Value2](f: Value => ?[Value2]): Builder[Value2, S] =
      new Builder[Value2, S](
        stateToValue = stateToValue(_).flatMap(f),
        stateToNode = stateToNode,
      )

    // ---  ---

    def mapErrorsWithState(f: (Throwable, S) => Throwable): Builder[Value, S] =
      new Builder[Value, S](
        stateToValue = s => stateToValue(s).mapErrors(f(_, s)),
        stateToNode = stateToNode,
      )

    def mapWithState[Value2](f: (Value, S) => Value2): Builder[Value2, S] =
      new Builder[Value2, S](
        stateToValue = s => stateToValue(s).map(f(_, s)),
        stateToNode = stateToNode,
      )

    def flatMapWithState[Value2](f: (Value, S) => ?[Value2]): Builder[Value2, S] =
      new Builder[Value2, S](
        stateToValue = s => stateToValue(s).flatMap(f(_, s)),
        stateToNode = stateToNode,
      )

  }

  def apply[Value, S](stateToValue: S => ?[Value])(stateToNode: S => Frag): Builder[Value, S] =
    new Builder[Value, S](
      stateToValue = stateToValue,
      stateToNode = stateToNode,
    )

}
