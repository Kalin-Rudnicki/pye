package klib.webServer.widgets

import scalatags.JsDom.all.Frag

import klib.Implicits._
import klib.fp.types._

trait GWidget[Value, S <: Widget.State[Value]] {
  val node: Frag
  val state: S
}

object Widget {

  trait State[Value] {
    def to_? : ?[Value]
  }

  def apply[Value](
      _node: Frag,
      _state: State[Value],
  ): Widget[Value] =
    new Widget[Value] {
      override val node: Frag = _node
      override val state: State[Value] = _state
    }

  // =====| Sort of TypeClass stuff |=====

  def map[A, B](widget: Widget[A])(f: A => B): Widget[B] =
    Widget(
      widget.node,
      new State[B] { override def to_? : ?[B] = widget.state.to_?.map(f) },
    )

  def flatMap[A, B](widget: Widget[A])(f: A => ?[B]): Widget[B] =
    Widget(
      widget.node,
      new State[B] { override def to_? : ?[B] = widget.state.to_?.flatMap(f) },
    )

}
