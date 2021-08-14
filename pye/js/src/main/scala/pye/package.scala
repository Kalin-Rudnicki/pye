//

import scala.scalajs.js

import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import pye.Implicits._

package object pye {

  def throwableSourceMapReference(throwable: Throwable): IndentedString = {
    import IndentedString._

    inline(
      Maybe(throwable.getMessage).cata(msg => s"$msg (${throwable.getClass})", throwable.toString),
      "stack-trace:",
      indented(
        throwable.getStackTrace.map { ste =>
          facades.wrappers.sourceMap.SourceMapData.allPositionsForLine(ste.getLineNumber).toList.toNel match {
            case Some(hints) =>
              val hintsArray = hints.toList.toArray
              val maxFileNameLen = hintsArray.map(_._1.length).max

              inline(
                s"> ${ste.getClassName}.${ste.getMethodName}",
                indented(
                  hintsArray.map {
                    case (fileName, lineNos) =>
                      s"~ ${fileName.rightAlign(maxFileNameLen)}: ${lineNos.mkString(", ")}"
                  }.toList,
                ),
              )
            case None =>
              inline(
                "@",
                indented(
                  s"~ ${ste.getFileName}:${ste.getLineNumber}",
                  s"~ ${ste.getClassName}.${ste.getMethodName}",
                ),
              )
          }
        }.toList,
      ),
      Maybe(throwable.getCause).map { cause =>
        inline(
          "cause:",
          indented(
            throwableSourceMapReference(cause),
          ),
        )
      },
    )
  }

  def displayMessage(msg: Raise.DisplayMessage): Unit = {
    def getElement(id: String): Maybe[Element] = Maybe(document.getElementById(id))
    def globalMessages: Maybe[Element] = getElement(Page.names.PageMessages)

    msg.causeId.cata(causeId => getElement(s"$causeId-messages"), globalMessages) match {
      case Some(messagesElement) =>
        val messageElement =
          div(
            msg.message,
            msg.modifier,
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

  def makeWebPage(
      onLoad: AsyncIO[Maybe[Page]],
      routeMatcher: RouteMatcher,
  ): Unit = {
    for {
      _ <- AsyncIO { routeMatcher.bindToWindow() }
      mRedirect <- onLoad
      _ <- mRedirect match {
        case Some(redirect) =>
          // TODO (KR) :
          redirect.replace
        case None =>
          // TODO (KR) :
          AsyncIO { routeMatcher.attemptToLoadPage() }
      }
    } yield ()
  }.runAndShowErrors()

}
