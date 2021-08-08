package pye

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.URIUtils

import org.scalajs.dom._
import org.scalajs.dom.{html => H}
import scalatags.JsDom.all._
import scalatags.JsDom.tags2

import klib.Implicits._
import klib.fp.types._
import pye.Implicits._
import pye.widgets.modifiers.PyeS

final class Page[Env] private[Page] (
    path: String,
    envF: () => AsyncIO[Env],
    titleF: Env => String,
    bodyF: Env => H.Body,
    errorHandler: ErrorHandler,
    keyMap: KeyMap,
) {

  def withKeyMap(kmF: ErrorHandler => KeyMap => KeyMap): Page[Env] =
    new Page(
      path = path,
      envF = envF,
      titleF = titleF,
      bodyF = bodyF,
      errorHandler = errorHandler,
      keyMap = kmF(errorHandler)(keyMap),
    )

  private def _renderAnd(and: Env => Unit): AsyncIO[Unit] =
    for {
      env <- envF()
      _ <- AsyncIO {
        val title = titleF(env)

        window.document.title = title
        window.document.body = bodyF(env)
        and(env)
        keyMap.bindToWindow()
      }
    } yield ()

  private[pye] def _push(): AsyncIO[Unit] =
    _renderAnd { env =>
      window.history.pushState(null, titleF(env), path)
    }

  private[pye] def _replace(): AsyncIO[Unit] =
    _renderAnd { env =>
      window.history.replaceState(null, titleF(env), path)
    }

  private[pye] def _replaceNoTrace(): AsyncIO[Unit] =
    _renderAnd { _ => }

  // TODO (KR) : Remove `_`, and remove deprecated

  @deprecated(message = "Use new widget framework to change pages", since = "2.0.2")
  def renderAnd(and: Env => Unit): Unit = {
    for {
      _ <- _renderAnd(and)
      _ <- AsyncIO(console.log("Stop using the deprecated page-changer"))
    } yield ()
  }.runAndShowErrors()

  @deprecated(message = "Use new widget framework to change pages", since = "2.0.2")
  def push(): Unit =
    renderAnd { env =>
      window.history.pushState(null, titleF(env), path)
    }

  @deprecated(message = "Use new widget framework to change pages", since = "2.0.2")
  def replace(): Unit =
    renderAnd { env =>
      window.history.replaceState(null, titleF(env), path)
    }

  @deprecated(message = "Use new widget framework to change pages", since = "2.0.2")
  def replaceNoTrace(): Unit =
    renderAnd { _ => }

}
object Page {

  object Standard {

    object names {

      val PyeStandardStyle = "pye-standard-style"

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

  // =====|  |=====

  sealed trait NavBarItem
  object NavBarItem {

    final case class Button(
        action: NavBarAction,
        modifiers: Seq[Modifier],
    ) extends NavBarItem
    object Button {
      def build(action: NavBarAction, modifiers: Modifier*): Button =
        Button(action, modifiers)
    }

  }

  sealed trait NavBarAction
  object NavBarAction {
    final case class PushPage(page: Page[_]) extends NavBarAction
    final case class Custom(onClick: MouseEvent => Unit) extends NavBarAction
  }

  // =====|  |=====

  object builder {

    def path(paths: String*)(params: (String, String)*): Builder1 = {
      val pathString =
        paths
          .map(URIUtils.encodeURIComponent)
          .mkString("/")
      val paramString =
        params
          .map(p => s"${URIUtils.encodeURIComponent(p._1)}=${URIUtils.encodeURIComponent(p._2)}")
          .mkString("&")

      new Builder1(s"/pages/$pathString${params.nonEmpty ? s"?$paramString" | ""}")
    }

  }

  final class StandardBuilder1[Env] private[Page] {

    def pageCenterMiddle(build: Env => H.Div): StandardBuilder2[Env] =
      new StandardBuilder2[Env](
        _pageCenterMiddle = build,
        _pageTop = None,
        _pageBottom = None,
        _pageLeft = None,
        _pageRight = None,
        _pageCenterTop = None,
        _pageCenterBottom = None,
        _inBody = Nil,
      )

  }

  final class StandardBuilder2[Env] private[Page] (
      private[Page] val _pageCenterMiddle: Env => H.Div,
      private[Page] val _pageTop: Maybe[Env => (String, H.Div)],
      private[Page] val _pageBottom: Maybe[Env => (String, H.Div)],
      private[Page] val _pageLeft: Maybe[Env => H.Div],
      private[Page] val _pageRight: Maybe[Env => H.Div],
      private[Page] val _pageCenterTop: Maybe[Env => H.Div],
      private[Page] val _pageCenterBottom: Maybe[Env => H.Div],
      private[Page] val _inBody: List[Env => List[Modifier]],
  ) {

    private def copy(
        _pageCenterMiddle: Env => H.Div = this._pageCenterMiddle,
        _pageTop: Maybe[Env => (String, H.Div)] = this._pageTop,
        _pageBottom: Maybe[Env => (String, H.Div)] = this._pageBottom,
        _pageLeft: Maybe[Env => H.Div] = this._pageLeft,
        _pageRight: Maybe[Env => H.Div] = this._pageRight,
        _pageCenterTop: Maybe[Env => H.Div] = this._pageCenterTop,
        _pageCenterBottom: Maybe[Env => H.Div] = this._pageCenterBottom,
        _inBody: Maybe[Env => List[Modifier]] = None,
    ): StandardBuilder2[Env] =
      new StandardBuilder2[Env](
        _pageCenterMiddle = _pageCenterMiddle,
        _pageTop = _pageTop,
        _pageBottom = _pageBottom,
        _pageLeft = _pageLeft,
        _pageRight = _pageRight,
        _pageCenterTop = _pageCenterTop,
        _pageCenterBottom = _pageCenterBottom,
        _inBody = _inBody.cata(_ :: this._inBody, this._inBody),
      )

    def noPageTop: StandardBuilder2[Env] = copy(_pageTop = None)
    def pageTop(height: String)(build: Env => H.Div): StandardBuilder2[Env] =
      copy(_pageTop = Some(env => (height, build(env))))
    def navBarTop(height: String)(items: Env => (List[NavBarItem], List[NavBarItem])): StandardBuilder2[Env] =
      pageTop(height) { env =>
        def convertNavBarItem(navBarItem: NavBarItem): Element =
          navBarItem match {
            case NavBarItem.Button(action, modifiers) =>
              div(PyeS.`pye:nav-bar`.item)(
                onclick := { (e: MouseEvent) =>
                  action match {
                    case NavBarAction.PushPage(page) =>
                      // TODO (KR) : New Widget Actions
                      page.push()
                    case NavBarAction.Custom(onClick) =>
                      onClick(e)
                  }
                },
              )(modifiers).render
          }

        val (left, right) = items(env)

        div(PyeS.`pye:nav-bar`)(
          div(PyeS.`pye:nav-bar`.e(_.section).m(_.wrap))(
            left.map(convertNavBarItem),
          ),
          div(PyeS.`pye:nav-bar`.e(_.section).m(_.expand)),
          div(PyeS.`pye:nav-bar`.e(_.section).m(_.wrap))(
            right.map(convertNavBarItem),
          ),
        ).render
      }

    def noPageBottom: StandardBuilder2[Env] = copy(_pageBottom = None)
    def pageBottom(height: String)(build: Env => H.Div): StandardBuilder2[Env] =
      copy(_pageBottom = Some(env => (height, build(env))))

    def noPageLeft: StandardBuilder2[Env] = copy(_pageLeft = None)
    def pageLeft(build: Env => H.Div): StandardBuilder2[Env] =
      copy(_pageLeft = Some(build(_)))

    def noPageRight: StandardBuilder2[Env] = copy(_pageRight = None)
    def pageRight(build: Env => H.Div): StandardBuilder2[Env] =
      copy(_pageRight = Some(build(_)))

    def noPageCenterTop: StandardBuilder2[Env] = copy(_pageCenterTop = None)
    def pageCenterTop(build: Env => H.Div): StandardBuilder2[Env] =
      copy(_pageCenterTop = Some(build(_)))

    def noPageCenterBottom: StandardBuilder2[Env] = copy(_pageCenterBottom = None)
    def pageCenterBottom(build: Env => H.Div): StandardBuilder2[Env] =
      copy(_pageCenterBottom = Some(build(_)))

    def inBody(build: Env => List[Modifier]): StandardBuilder2[Env] =
      copy(_inBody = build.some)

  }

  final class Builder1 private[Page] (
      path: String,
  ) {

    def noEnv: Builder2[Unit] =
      new Builder2[Unit](
        path = path,
        envF = () => ().pure[AsyncIO],
      )

    def env[Env](envF: => AsyncIO[Env]): Builder2[Env] =
      new Builder2[Env](
        path = path,
        envF = () => envF,
      )

  }

  final class Builder2[Env] private[Page] (
      path: String,
      envF: () => AsyncIO[Env],
  ) {

    def constName(title: String): Builder3[Env] =
      new Builder3[Env](
        path = path,
        envF = envF,
        titleF = _ => title,
      )

    def name(titleF: Env => String): Builder3[Env] =
      new Builder3[Env](
        path = path,
        envF = envF,
        titleF = titleF,
      )

  }

  final class Builder3[Env] private[Page] (
      path: String,
      envF: () => AsyncIO[Env],
      titleF: Env => String,
  ) {

    def standardBody(bodyF: ErrorHandler => StandardBuilder1[Env] => StandardBuilder2[Env]): Page[Env] = {
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

      val sb = bodyF(errorHandler)(new StandardBuilder1[Env])

      new Page[Env](
        path = path,
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

          val currentStyles = document.head.getElementsByClassName(N.PyeStandardStyle)
          0.until(currentStyles.length).foreach { i =>
            document.head.removeChild(currentStyles(i))
          }
          document.head.insertBefore(
            tags2
              .style(`class` := N.PyeStandardStyle)(
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
          )(
            sb._inBody.reverseMap(_(env)).flatten: _*,
          ).render
        },
        errorHandler = errorHandler,
        keyMap = KeyMap.empty,
      )
    }

    def errorHandler(errorHandler: ErrorHandler): Builder4[Env] =
      new Builder4[Env](
        path = path,
        envF = envF,
        titleF = titleF,
        errorHandler = errorHandler,
      )

  }

  final class Builder4[Env] private[Page] (
      path: String,
      envF: () => AsyncIO[Env],
      titleF: Env => String,
      errorHandler: ErrorHandler,
  ) {

    def body(bodyF: (Env, ErrorHandler) => H.Body): Page[Env] =
      new Page[Env](
        path = path,
        envF = envF,
        titleF = titleF,
        errorHandler = errorHandler,
        bodyF = bodyF(_, errorHandler),
        keyMap = KeyMap.empty,
      )

  }

}
