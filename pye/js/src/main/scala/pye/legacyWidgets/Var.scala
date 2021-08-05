package pye.legacyWidgets

import scala.collection.mutable

@deprecated(message = "Use new Widget", since = "3.0.0")
final class Var[V] private (initialValue: V) {

  private val subscribed: mutable.ListBuffer[Widget[_]] = mutable.ListBuffer()
  def subscribe(widget: Widget[_]): widget.type = {
    subscribed.append(widget)
    widget
  }

  var _value: V = initialValue

  def silentValue: V = _value
  def silentValue_=(v: V): Unit =
    _value = v

  def value: V = _value
  def value_=(v: V): Unit = {
    _value = v
    subscribed.foreach(_.reRender())
  }

}
object Var {

  def apply[V](initialValue: V): Var[V] =
    new Var(initialValue)

}
