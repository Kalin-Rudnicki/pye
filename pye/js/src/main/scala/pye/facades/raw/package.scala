package pye.facades

import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js

import org.scalajs.dom._

package object raw {

  @js.native
  @JSGlobal
  class ResizeObserver(callback: js.Function2[js.Array[ResizeObserverEntry], ResizeObserver, Unit]) extends js.Object {
    def disconnect(): Unit = js.native
    def observe(target: Element): Unit = js.native
    def unobserve(target: Element): Unit = js.native
  }

  @js.native
  trait ResizeObserverEntry extends js.Object {
    def borderBoxSize: js.Array[ResizeObserverSize]
    def contentBoxSize: js.Array[ResizeObserverSize]
    def contentRect: DOMRectReadOnly
    def target: Element
  }

  @js.native
  trait ResizeObserverSize extends js.Object {
    def blockSize: Int
    def inlineSize: Int
  }

  @js.native
  trait DOMRectReadOnly extends js.Object {
    def bottom: Double
    def height: Double
    def left: Double
    def right: Double
    def top: Double
    def width: Double
    def x: Double
    def y: Double
  }

}
