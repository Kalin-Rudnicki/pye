package klib.webServer.widgets

import scalatags.JsDom.all.Frag

import klib.fp.types._

trait GWidget[Value, S <: GWidget.State[Value]] {
  val node: Frag
  val state: S
}
object GWidget {

  trait State[Value] {
    def to_? : ?[Value]
  }

  def apply[Value, S <: State[Value]](
      _node: Frag,
      _state: S,
  ): GWidget[Value, S] =
    new GWidget[Value, S] {
      override val node: Frag = _node
      override val state: S = _state
    }

}
