package klib.webServer.widgets

import org.scalajs.dom.Event

trait events {

  def submitEvent: Event =
    new Event(
      "submit",
      new org.scalajs.dom.raw.EventInit {
        bubbles = true
      },
    )

}
object events extends events
