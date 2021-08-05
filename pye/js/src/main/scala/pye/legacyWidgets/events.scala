package pye.legacyWidgets

import org.scalajs.dom.Event

trait events {

  def submitEvent: Event =
    new Event(
      "submit",
      new org.scalajs.dom.raw.EventInit {
        bubbles = true
      },
    )

  def closeModalEvent: Event =
    new Event(
      "close-modal",
      new org.scalajs.dom.raw.EventInit {
        bubbles = true
      },
    )

}
object events extends events
