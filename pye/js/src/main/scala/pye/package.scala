//

import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._

package object pye {

  type ErrorHandler = Throwable => Unit

  def displayMessage(msg: Raise.DisplayMessage): Unit = {
    def getElement(id: String): Maybe[Element] = Maybe(document.getElementById(id))
    def globalMessages: Maybe[Element] = getElement(Page.Standard.names.PageMessages)

    msg.causeId.cata(causeId => getElement(s"$causeId-messages"), globalMessages) match {
      case Some(messagesElement) =>
        val messageElement =
          div(
            msg.message,
            msg.modifiers,
          ).render

        messagesElement.appendChild(messageElement)
        val timeoutId =
          msg.timeout.map {
            window.setTimeout(
              () => {
                messagesElement.removeChild(messageElement)
              },
              _,
            )
          }

        messageElement.onclick = { _ =>
          timeoutId.foreach(window.clearTimeout)
          messagesElement.removeChild(messageElement)
        }
      case None =>
        // TODO (KR) :
        window.alert(msg.message)
    }
  }

}
