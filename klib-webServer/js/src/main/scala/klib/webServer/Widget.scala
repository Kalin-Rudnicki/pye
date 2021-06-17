package klib.webServer

import scalatags.JsDom.all.Frag

import klib.Implicits._
import klib.fp.types._

trait Widget[Value] {
  val node: Frag
  def valueF: ?[Value]
}

object Widget {

  def apply[Value](
      _node: Frag,
      _valueF: => ?[Value],
  ): Widget[Value] =
    new Widget[Value] {
      override val node: Frag = _node
      override def valueF: ?[Value] = _valueF
    }

  // =====| Sort of TypeClass stuff |=====

  def map[A, B](widget: Widget[A])(f: A => B): Widget[B] =
    Widget(
      widget.node,
      widget.valueF.map(f),
    )

  def flatMap[A, B](widget: Widget[A])(f: A => ?[B]): Widget[B] =
    Widget(
      widget.node,
      widget.valueF.flatMap(f),
    )

}
