package pye.legacyWidgets

import org.scalajs.dom.Event

trait events {

  @deprecated(message = "Use new Widget", since = "3.0.0")
  def submitEvent: Event =
    new Event(
      "submit",
      new org.scalajs.dom.raw.EventInit {
        bubbles = true
      },
    )

  @deprecated(message = "Use new Widget", since = "3.0.0")
  def closeModalEvent: Event =
    new Event(
      "close-modal",
      new org.scalajs.dom.raw.EventInit {
        bubbles = true
      },
    )

}
object events extends events
