package klib.webServer

import scalatags.JsDom.all.Frag

import klib.Implicits._
import klib.fp.types._

package object widgets {

  type Widget[Value] = GWidget[Value, GWidget.State[Value]]
  object Widget {

    def apply[Value](
        _node: Frag,
        _state: GWidget.State[Value],
    ): Widget[Value] =
      new Widget[Value] {
        override val node: Frag = _node
        override val state: GWidget.State[Value] = _state
      }

    // =====| Sort of TypeClass stuff |=====

    def map[A, B](widget: Widget[A])(f: A => B): Widget[B] =
      Widget(
        widget.node,
        new GWidget.State[B] { override def to_? : ?[B] = widget.state.to_?.map(f) },
      )

    def flatMap[A, B](widget: Widget[A])(f: A => ?[B]): Widget[B] =
      Widget(
        widget.node,
        new GWidget.State[B] { override def to_? : ?[B] = widget.state.to_?.flatMap(f) },
      )

  }

}
