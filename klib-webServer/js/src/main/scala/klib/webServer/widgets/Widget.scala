package klib.webServer.widgets

import klib.Implicits._
import klib.fp.types._
import org.scalajs.dom.raw.HTMLElement

final class Widget[Value, S] private (
    val state: S,
    val node: HTMLElement,
    _value: => ?[Value],
) {
  def value: ?[Value] = _value
}

object Widget {

  final class Builder[Value, S] private[Widget] (
      val stateToValue: S => ?[Value],
      val stateToNode: S => HTMLElement,
  ) {

    def apply(s: S): Widget[Value, S] =
      new Widget[Value, S](
        state = s,
        node = stateToNode(s),
        _value = stateToValue(s),
      )

    def mapNode(nodeF: HTMLElement => HTMLElement): Builder[Value, S] =
      new Builder[Value, S](
        stateToValue = stateToValue,
        stateToNode = s => nodeF(stateToNode(s)),
      )

    // NOTE : If using this, it is important that anything that is actually looking for changes in S2's state,
    //      : then S & S2 need to have the same reference to that.
    //      :
    //      : Ex: S1(var value: String)
    //      :     S2(var value: String)
    //      :     val s1 = S1("1")
    //      :     val s2 = S2(s1.value)
    //      :     s2.value = "2"
    //      ;     s1.value // => "1"
    //      ;     [BAD]
    //      :
    //      : Ex: S3(state: Var[String])
    //      :     S4(state: Var[String])
    //      :     val s3 = S3(Var("3"))
    //      :     val s4 = S4(s3.state)
    //      :     s4.state.value = "4"
    //      ;     s3.value // => "4"
    //      ;     [GOOD]
    def rMapState[S2](sF: S2 => S): Builder[Value, S2] =
      new Builder[Value, S2](
        stateToValue = s => stateToValue(sF(s)),
        stateToNode = s => stateToNode(sF(s)),
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

  def apply[Value, S](stateToValue: S => ?[Value])(stateToNode: S => HTMLElement): Builder[Value, S] =
    new Builder[Value, S](
      stateToValue = stateToValue,
      stateToNode = stateToNode,
    )

}
