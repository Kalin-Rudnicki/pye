package klib.webServer.widgets

final class Var[V] private (initialValue: V) { var value: V = initialValue }
object Var {
  def apply[V](initialValue: V): Var[V] = new Var(initialValue)
}
