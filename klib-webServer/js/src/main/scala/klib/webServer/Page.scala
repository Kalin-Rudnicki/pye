package klib.webServer

import scala.concurrent.ExecutionContext.Implicits.global

import org.scalajs.dom._
import org.scalajs.dom.html._
import scalatags.JsDom.all._
import scalatags.JsDom.tags2

import klib.Implicits._
import klib.fp.types._

trait Page[Env] {

  protected val keyMap: KeyMap =
    new KeyMap

  // =====| ... |=====

  def render(): Unit =
    loadEnv.future.onComplete {
      _.to_?.flatten match {
        case Alive(env) =>
          document.title = this.pageTitle(env)
          document.body = this.pageBody(env)
          keyMap.bindTo(window)
        case Dead(errors) =>
          errors.foreach(handleError)
      }
    }

  // =====| ... |=====

  protected def loadEnv: HttpResponse[Env]

  protected def pageTitle(env: Env): String
  protected def pageBody(env: Env): Body

  protected def handleError(error: Throwable): Unit

}

object Page {

  trait Standard[Env] extends Page[Env] {
    import Standard.{names => N}

    /*
    /* --- CSS Template --- */

    #page {}
    .page-bar {}
    .page-body {}

    /* First Group */
    #page-top {}
    #page-middle {} /* Probably best to leave alone... */
    #page-bottom {}

    /* Second Group */
    #page-left {}
    #page-center {} /* Probably best to leave alone... */
    #page-right {}

    /* Third Group */
    #page-errors {}
    #page-center-top {}
    #page-center-middle {}
    #page-center-bottom {}

     */

    // =====| ... |=====

    final override protected def pageBody(env: Env): Body = {
      // --- Get Items ---
      val pageTop = this.pageTop(env)
      val pageBottom = this.pageBottom(env)

      val pageLeft = this.pageLeft(env)
      val pageRight = this.pageRight(env)

      val pageCenterTop = this.pageCenterTop(env)
      val pageCenterMiddle = this.pageCenterMiddle(env)
      val pageCenterBottom = this.pageCenterBottom(env)

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
               | #${N.PageErrors} { flex-shrink: 0; }
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
              div(id := N.PageErrors),
              pageCenterTop.toOption,
              pageCenterMiddle,
              pageCenterBottom.toOption,
            ),
            pageRight.toOption,
          ),
          pageBottom.map(_._2).toOption,
        ),
      ).render
    }

    final override protected def handleError(error: Throwable): Unit =
      Maybe(document.getElementById(N.PageErrors)) match {
        case Some(pageErrors) =>
          pageErrors.appendChild(this.errorToNode(error))
        case None =>
          // TODO (KR) : Possibly improve this (?)
          console.error(error)
      }

    // =====| ... |=====

    def errorToNode(error: Throwable): Node

    /**
      * NOTE : Do not set the `id` attribute on any of the `page___` methods (it will be clobbered).
      *      : They will automatically be set to their given name in `Page.Standard.names`.
      *       : Top/bottom bars are given the class page-bar, and (left/right/center-middle) are given page-body
      *      : Also, any implementation of top/bottom must include their heights (ex: 30px) as a String.
      */
    def pageCenterMiddle(env: Env): Div

    def pageTop(env: Env): Maybe[(String, Div)] = None
    def pageBottom(env: Env): Maybe[(String, Div)] = None

    def pageLeft(env: Env): Maybe[Div] = None
    def pageRight(env: Env): Maybe[Div] = None

    def pageCenterTop(env: Env): Maybe[Div] = None
    def pageCenterBottom(env: Env): Maybe[Div] = None

  }
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

      val PageErrors = "page-errors"
      val PageCenterTop = "page-center-top"
      val PageCenterMiddle = "page-center-middle"
      val PageCenterBottom = "page-center-bottom"

      val Modal = "modal"

    }

  }

}
