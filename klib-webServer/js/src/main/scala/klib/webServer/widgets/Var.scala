package klib.webServer.widgets

import klib.Implicits._
import klib.fp.types._

final class Var[V] private (initialValue: V, var onChange: Maybe[V => Unit]) {

  var _value: V = initialValue

  def value: V = _value
  def value_=(v: V): Unit = {
    _value = v
    onChange.foreach(_(_value))
  }

}
object Var {

  def apply[V](
      initialValue: V,
      onChange: Maybe[V => Unit] = None,
  ): Var[V] =
    new Var(initialValue, onChange)

}
