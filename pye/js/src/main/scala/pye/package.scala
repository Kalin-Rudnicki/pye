//

import scala.scalajs.js

import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import klib.utils._

package object pye {

  // REMOVE : ...
  type ErrorHandler = Throwable => Unit

  def logThrowable(throwable: Throwable, indent: String = "    "): AsyncIO[Unit] = {
    def getAllFiles(throwable: Throwable): Set[String] = {
      val mine = throwable.getStackTrace.map(_.getFileName).toSet
      Maybe(throwable.getCause).cata(c => getAllFiles(c) | mine, mine)
    }

    def convertThrowable(
        sourceMaps: Map[String, facades.wrappers.sourceMap.SourceMapConsumer],
        throwable: Throwable,
    ): IndentedString = {
      import IndentedString._

      val mappedStackTrace =
        throwable.getStackTrace.map { ste =>
          sourceMaps.get(ste.getFileName).toMaybe.flatMap(_.originalPositionFor(ste.getLineNumber)) match {
            case Some(orig) =>
              (
                orig.source.split("/src/main/[^/]+/") match {
                  case Array(_, path) => path.replaceAllLiterally("/", ".")
                  case _              => orig.source
                },
                orig.line,
              ).right
            case None =>
              ste.left
          }
        }
      val maxMappedFileNameLength =
        mappedStackTrace
          .collect {
            case Right((fileName, _)) =>
              fileName
          }
          .maxByOption(_.length)
          .toMaybe
          .cata(_.length, 0)

      inline(
        Maybe(throwable.getMessage).cata(msg => s"$msg (${throwable.getClass})", throwable.toString),
        "stack-trace:",
        indented(
          mappedStackTrace.map {
            case Right((fileName, lineNo)) =>
              inline(
                s"> ${" " * (maxMappedFileNameLength - fileName.length)}$fileName: $lineNo",
              )
            case Left(ste) =>
              inline(
                ">",
                indented(
                  s"~ ${ste.getFileName}",
                  s"~ ${ste.getClassName}",
                  s"~ ${ste.getLineNumber.toString}",
                ),
              )
          }.toList,
        ),
        Maybe(throwable.getCause).map { throwable =>
          inline(
            "cause:",
            indented(
              convertThrowable(sourceMaps, throwable),
            ),
          )
        },
      )
    }

    for {
      sourceMaps1 <- getAllFiles(throwable).toList.map { fileName =>
        HttpRequest("GET", s"$fileName.map").noBody.raw200.map { text =>
          (
            fileName,
            js.JSON.parse(text),
          )
        }.unLift
      }.traverse
      sourceMaps2 = sourceMaps1.collect { case Alive(r) => r }

      _ <-
        sourceMaps2
          .map {
            case (fileName, json) =>
              facades.wrappers.sourceMap.SourceMapConsumer.callNew(json).map((fileName, _))
          }
          .traverse
          .bracket { sourceMaps =>
            {
              for {
                convertedMessage <- IO { convertThrowable(sourceMaps.toMap, throwable) }
                _ <- IO { console.log(convertedMessage.toString(indent)) }
              } yield ()
            }.toAsyncIO
          } { sourceMaps =>
            IO { sourceMaps.foreach(_._2.destroy()) }.toAsyncIO
          }
    } yield ()
  }

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

  def displayMessage2(msg: Raise2.DisplayMessage): Unit = {
    // TODO (KR) :
    ???
  }

}
