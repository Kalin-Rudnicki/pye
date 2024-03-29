//

import scala.scalajs.js

import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import pye.Implicits._
import pye.Raise.History

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

    // TODO (KR) : Could possibly use some improvement

    val messagesElement =
      msg.causeId
        .cata(causeId => getElement(s"$causeId-messages"), globalMessages)
        .getOrElse(document.body)

    val messageElement =
      div(
        msg.message,
        msg.modifier,
      ).render

    if (messagesElement.children.length == 0)
      messagesElement.appendChild(messageElement)
    else
      messagesElement.insertBefore(messageElement, messagesElement.children(0))
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
  }

  private val toleranceMap: Map[String, Logger.LogLevel with Logger.LogLevel.Tolerance] =
    Logger.LogLevel.AllTolerance.flatMap { l =>
      List(l.name, l.displayName).map { n => (n.toUpperCase, l) }
    }.toMap

  private var _PyeLogger: Logger = _
  def PyeLogger: Logger = _PyeLogger

  // NOTE : This is for usage in the javascript console
  @js.annotation.JSExportTopLevel("setLogTolerance")
  def setLogTolerance(name: String): Unit =
    toleranceMap.get(name.toUpperCase).toMaybe match {
      case Some(logTolerance) =>
        _PyeLogger = _PyeLogger.withDefaultLogTolerance(logTolerance)
      case None =>
        displayMessage(Raise.DisplayMessage.global.error(s"Invalid LogLevel: $name"))
    }

  @js.annotation.JSExportTopLevel("includeLoggerFlags")
  def includeLoggerFlags(flags: String*): Unit =
    _PyeLogger = _PyeLogger.includingDefaultFlags(flags: _*)

  @js.annotation.JSExportTopLevel("excludeLoggerFlags")
  def excludeLoggerFlags(flags: String*): Unit =
    _PyeLogger = _PyeLogger.excludingDefaultFlags(flags: _*)

  def makeWebPage(
      onLoad: AsyncIO[List[Raise.Standard]],
      routeMatcher: RouteMatcher,
      logger: Logger = Logger(Logger.LogLevel.Info),
  ): Unit = {
    def handle(raises: List[Raise.Standard]): AsyncIO[Boolean] =
      raises match {
        case head :: tail =>
          for {
            r1 <- head match {
              case msg: Raise.DisplayMessage =>
                AsyncIO { displayMessage(msg); false }
              case history: Raise.History =>
                history match {
                  case History.Push(page) =>
                    page().push.map { _ => true }
                  case History.Replace(page) =>
                    page().replace.map { _ => true }
                  case History.Go(_) =>
                    // Ignore (?)
                    AsyncIO { false }
                }
              case Raise.RefreshPage =>
                // Ignore (?)
                AsyncIO { false }
              case Raise.Raw(action, _) =>
                action.map { _ => false }
            }
            r2 <- handle(tail)
          } yield r1 || r2
        case Nil =>
          AsyncIO { false }
      }

    for {
      _ <- AsyncIO {
        _PyeLogger = logger
          .withDefaultColorMode(Logger.ColorMode.Simple)
          .withFlagMap(
            Map(
              "all" -> InfiniteSet.Exclusive(),
              "applied-widget" -> InfiniteSet.Inclusive("value", "current", "getElementsAndUpdate"),
              "widget" -> InfiniteSet.Inclusive("convert"),
            ),
          )
      }
      _ <- AsyncIO { routeMatcher.bindToWindow() }
      raises <- onLoad
      redirected <- handle(raises)
      // TODO (KR) : Is this the issue (?)
      _ <- redirected ? AsyncIO {} | AsyncIO { routeMatcher.attemptToLoadPage() }
    } yield ()
  }.runAndShowErrors()

}
