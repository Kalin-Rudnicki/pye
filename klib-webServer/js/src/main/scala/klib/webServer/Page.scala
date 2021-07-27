package klib.webServer

import scala.concurrent.ExecutionContext.Implicits.global

import org.scalajs.dom._
import org.scalajs.dom.html._
import scalatags.JsDom.all._
import scalatags.JsDom.tags2

import klib.Implicits._
import klib.fp.types._

final class Page[Env] private[Page] (
    envF: () => HttpResponse[Env],
    titleF: Env => String,
    bodyF: Env => Body,
    errorHandler: ErrorHandler,
) {

  def render(): Unit = {
    implicit val eh: ErrorHandler = errorHandler
    envF().onComplete { env =>
      window.document.title = titleF(env)
      window.document.body = bodyF(env)
    }
  }

}
object Page {

  object Standard {

    object names {

      val KWSStandardStyle = "klib-webserver-standard-style"

      val PageTopHeight = "--page-top-height"
      val PageBottomHeight = "--page-bottom-height"

      val Page = "page"
      val PageBar = "page-bar"
      val PageBody = "page-body"

      val PageTop = "page-top"
      val PageMiddle = "page-middle" // NOTE : This is just an intermediate container
      val PageBottom = "page-bottom"

      val PageLeft = "page-left"
      val PageCenter = "page-center" // NOTE : This is just an intermediate container
      val PageRight = "page-right"

      val PageMessages = "page-messages"
      val PageMessages_Error = "page-messages__error"

      val PageCenterTop = "page-center-top"
      val PageCenterMiddle = "page-center-middle"
      val PageCenterBottom = "page-center-bottom"

      val Modal = "modal"

    }

  }

  object builder {

    def noEnv: Builder1[Unit] =
      new Builder1[Unit](
        envF = () => ().pure[HttpResponse],
      )

    def env[Env](envF: => HttpResponse[Env]): Builder1[Env] =
      new Builder1[Env](
        envF = () => envF,
      )

  }

  final class StandardBuilder1[Env] private[Page] {

    def pageCenterMiddle(build: Env => Div): StandardBuilder2[Env] =
      new StandardBuilder2[Env](
        _pageCenterMiddle = build,
        _pageTop = None,
        _pageBottom = None,
        _pageLeft = None,
        _pageRight = None,
        _pageCenterTop = None,
        _pageCenterBottom = None,
      )

  }

  final class StandardBuilder2[Env] private[Page] (
      private[Page] val _pageCenterMiddle: Env => Div,
      private[Page] val _pageTop: Maybe[Env => (String, Div)],
      private[Page] val _pageBottom: Maybe[Env => (String, Div)],
      private[Page] val _pageLeft: Maybe[Env => Div],
      private[Page] val _pageRight: Maybe[Env => Div],
      private[Page] val _pageCenterTop: Maybe[Env => Div],
      private[Page] val _pageCenterBottom: Maybe[Env => Div],
  ) {

    private def copy(
        _pageCenterMiddle: Env => Div = this._pageCenterMiddle,
        _pageTop: Maybe[Env => (String, Div)] = this._pageTop,
        _pageBottom: Maybe[Env => (String, Div)] = this._pageBottom,
        _pageLeft: Maybe[Env => Div] = this._pageLeft,
        _pageRight: Maybe[Env => Div] = this._pageRight,
        _pageCenterTop: Maybe[Env => Div] = this._pageCenterTop,
        _pageCenterBottom: Maybe[Env => Div] = this._pageCenterBottom,
    ): StandardBuilder2[Env] =
      new StandardBuilder2[Env](
        _pageCenterMiddle = _pageCenterMiddle,
        _pageTop = _pageTop,
        _pageBottom = _pageBottom,
        _pageLeft = _pageLeft,
        _pageRight = _pageRight,
        _pageCenterTop = _pageCenterTop,
        _pageCenterBottom = _pageCenterBottom,
      )

    def noPageTop: StandardBuilder2[Env] = copy(_pageTop = None)
    def pageTop(height: String)(build: Env => Div): StandardBuilder2[Env] =
      copy(_pageTop = Some(env => (height, build(env))))

    def noPageBottom: StandardBuilder2[Env] = copy(_pageBottom = None)
    def pageBottom(height: String)(build: Env => Div): StandardBuilder2[Env] =
      copy(_pageBottom = Some(env => (height, build(env))))

    def noPageLeft: StandardBuilder2[Env] = copy(_pageLeft = None)
    def pageLeft(build: Env => Div): StandardBuilder2[Env] =
      copy(_pageLeft = Some(build(_)))

    def noPageRight: StandardBuilder2[Env] = copy(_pageRight = None)
    def pageRight(build: Env => Div): StandardBuilder2[Env] =
      copy(_pageRight = Some(build(_)))

    def noPageCenterTop: StandardBuilder2[Env] = copy(_pageCenterTop = None)
    def pageCenterTop(build: Env => Div): StandardBuilder2[Env] =
      copy(_pageCenterTop = Some(build(_)))

    def noPageCenterBottom: StandardBuilder2[Env] = copy(_pageCenterBottom = None)
    def pageCenterBottom(build: Env => Div): StandardBuilder2[Env] =
      copy(_pageCenterBottom = Some(build(_)))

  }

  final class Builder1[Env] private[Page] (
      envF: () => HttpResponse[Env],
  ) {

    def constName(title: String): Builder2[Env] =
      new Builder2[Env](
        envF = envF,
        titleF = _ => title,
      )

    def name(titleF: Env => String): Builder2[Env] =
      new Builder2[Env](
        envF = envF,
        titleF = titleF,
      )

  }

  final class Builder2[Env] private[Page] (
      envF: () => HttpResponse[Env],
      titleF: Env => String,
  ) {

    def standardBody(bodyF: ErrorHandler => KeyMap => StandardBuilder1[Env] => StandardBuilder2[Env]): Page[Env] = {
      import Standard.{names => N}

      val errorHandler: ErrorHandler = { error =>
        Maybe(document.getElementById(N.PageMessages)) match {
          case Some(pageMessages) =>
            val node =
              div(
                `class` := N.PageMessages_Error,
              )(
                error.toString,
              ).render

            node.onclick = _ => pageMessages.removeChild(node)

            pageMessages.appendChild(node)
          case None =>
            console.log(error.toString)
        }
      }
      // TODO (KR) :
      val keyMap = new KeyMap
      val sb = bodyF(errorHandler)(keyMap)(new StandardBuilder1[Env])

      new Page[Env](
        envF = envF,
        titleF = titleF,
        bodyF = env => {

          // --- Get Items ---
          val pageTop = sb._pageTop.map(_(env))
          val pageBottom = sb._pageBottom.map(_(env))

          val pageLeft = sb._pageLeft.map(_(env))
          val pageRight = sb._pageRight.map(_(env))

          val pageCenterTop = sb._pageCenterTop.map(_(env))
          val pageCenterMiddle = sb._pageCenterMiddle(env)
          val pageCenterBottom = sb._pageCenterBottom.map(_(env))

          // --- Add id's & classes ---
          pageTop.foreach { n => n._2.id = N.PageTop; n._2.classList.add(N.PageBar) }
          pageBottom.foreach { n => n._2.id = N.PageBottom; n._2.classList.add(N.PageBar) }

          pageLeft.foreach { n => n.id = N.PageLeft; n.classList.add(N.PageBody) }
          pageRight.foreach { n => n.id = N.PageRight; n.classList.add(N.PageBody) }

          pageCenterTop.foreach { n => n.id = N.PageCenterTop; n.classList.add(N.PageBar) }
          pageCenterMiddle.id = N.PageCenterMiddle; pageCenterMiddle.classList.add(N.PageBody)
          pageCenterBottom.foreach { n => n.id = N.PageCenterBottom; n.classList.add(N.PageBar) }

          // --- Style ---
          val pageTopHeight = pageTop.cata(_._1, "0px")
          val pageBottomHeight = pageBottom.cata(_._1, "0px")

          val currentStyles = document.head.getElementsByClassName(N.KWSStandardStyle)
          0.until(currentStyles.length).foreach { i =>
            document.head.removeChild(currentStyles(i))
          }
          document.head.insertBefore(
            tags2
              .style(`class` := N.KWSStandardStyle)(
                s"""
                   | :root { ${N.PageTopHeight}: $pageTopHeight; ${N.PageBottomHeight}: $pageBottomHeight; }
                   | body { margin: 0; padding: 0; }
                   | 
                   | #${N.Page} { height: 100vh; }
                   | #${N.PageTop} { height: var(--page-top-height); }
                   | #${N.PageMiddle} { display: flex; height: calc(100vh - var(--page-top-height) - var(--page-bottom-height)); }
                   | #${N.PageBottom} { height: var(--page-bottom-height); }
                   | #${N.PageLeft} { overflow-y: auto; flex-shrink: 0; }
                   | #${N.PageCenter} { display: flex; flex-direction: column; flex-grow: 1; }
                   | #${N.PageRight} { overflow-y: auto; flex-shrink: 0; }
                   | #${N.PageMessages} { flex-shrink: 0; }
                   | #${N.PageCenterTop} { flex-shrink: 0; }
                   | #${N.PageCenterMiddle} { flex-grow: 1; overflow-y: auto; }
                   | #${N.PageCenterBottom} { flex-shrink: 0; }
                   | 
                   | .${N.Modal} { display: block; position: fixed; left: 0; top: 0; width: 100vw; height: 100vh; background-color: rgb(0, 0, 0); background-color: rgba(0, 0, 0, 0.75); }
                   |""".stripMargin,
              )
              .render,
            document.head.children(0),
          )

          // --- Body ---
          body(
            // Page
            div(id := N.Page)(
              pageTop.map(_._2).toOption,
              div(id := N.PageMiddle)(
                pageLeft.toOption,
                div(id := N.PageCenter)(
                  div(id := N.PageMessages),
                  pageCenterTop.toOption,
                  pageCenterMiddle,
                  pageCenterBottom.toOption,
                ),
                pageRight.toOption,
              ),
              pageBottom.map(_._2).toOption,
            ),
          ).render
        },
        errorHandler = errorHandler,
      )
    }

    def errorHandler(errorHandler: ErrorHandler): Builder3[Env] =
      new Builder3[Env](
        envF = envF,
        titleF = titleF,
        errorHandler = errorHandler,
      )

  }

  final class Builder3[Env] private[Page] (
      envF: () => HttpResponse[Env],
      titleF: Env => String,
      errorHandler: ErrorHandler,
  ) {

    def body(bodyF: (Env, ErrorHandler) => Body): Page[Env] =
      new Page[Env](
        envF = envF,
        titleF = titleF,
        errorHandler = errorHandler,
        bodyF = bodyF(_, errorHandler),
      )

  }

}
