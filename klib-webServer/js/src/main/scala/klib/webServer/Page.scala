package klib.webServer

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.dom._
import org.scalajs.dom.html.Body
import org.scalajs.dom.html.Div
import scalatags.JsDom.all._
import scalatags.JsDom.tags2
import klib.Implicits._
import klib.fp.types._
import klib.webServer.Page.KeyMap.On

import scala.annotation.tailrec

trait Page[Env] {

  protected val keyMap: Page.KeyMap =
    new Page.KeyMap

  // =====| ... |=====

  def render(): Unit =
    loadEnv.future.onComplete {
      _.to_?.flatten match {
        case Alive(env) =>
          document.title = this.pageTitle(env)
          document.body = this.pageBody(env)
          keyMap.bindToWindow()
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

      // TODO (KR) : Maybe do something like document.findById("klib-webserver-styles") [!= null] .replaceWith(this)
      //           : Need to look into priority would work
      val _style =
        tags2
          .style(
            s"""
           | :root { ${N.PageTopHeight}: $pageTopHeight; ${N.PageBottomHeight}: $pageBottomHeight; }
           | body { margin: 0; padding: 0; }
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
           |""".stripMargin,
          )

      // --- Body ---
      body(
        // Style
        _style,
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

    }

  }

  final class KeyMap {
    import scala.collection.mutable

    // TODO (KR) : Add a binding for displaying keymap, or at least logging it...
    private val onDownKeys: mutable.ListBuffer[KeyMap.Key] = mutable.ListBuffer()
    private val onPressKeys: mutable.ListBuffer[KeyMap.Key] = mutable.ListBuffer()
    private val onUpKeys: mutable.ListBuffer[KeyMap.Key] = mutable.ListBuffer()

    private[webServer] def bindToWindow(): Unit = {
      def buildListener(lb: mutable.ListBuffer[KeyMap.Key])(e: KeyboardEvent): Unit = {
        @tailrec
        def loop(keys: List[KeyMap.Key]): Unit =
          keys match {
            case head :: tail =>
              if (head.matches(e)) {
                if (head.preventsDefault)
                  e.preventDefault()
                head.action(e)
              } else
                loop(tail)
            case Nil =>
          }

        loop(lb.toList)
      }

      window.onkeydown = buildListener(onDownKeys)(_)
      window.onkeypress = buildListener(onPressKeys)(_)
      window.onkeyup = buildListener(onUpKeys)(_)
    }

    // =====|  |=====

    def on(key: KeyMap.Key): this.type = {
      {
        key.on match {
          case On.KeyDown  => onDownKeys
          case On.KeyPress => onPressKeys
          case On.KeyUp    => onUpKeys
        }
      }.append(key)

      this
    }

    def onDown(
        key: String,
        name: String,
        ctrl: Maybe[Boolean] = false.some,
        shift: Maybe[Boolean] = false.some,
        preventsDefault: Boolean = true,
    )(action: KeyboardEvent => Unit): this.type =
      on(
        KeyMap.Key(
          keys = Set(key),
          name = name,
          action = action,
          ctrl = ctrl,
          shift = shift,
          on = On.KeyDown,
          preventsDefault = preventsDefault,
        ),
      )

    def onPress(
        key: String,
        name: String,
        ctrl: Maybe[Boolean] = false.some,
        shift: Maybe[Boolean] = false.some,
        preventsDefault: Boolean = true,
    )(action: KeyboardEvent => Unit): this.type =
      on(
        KeyMap.Key(
          keys = Set(key),
          name = name,
          action = action,
          ctrl = ctrl,
          shift = shift,
          on = On.KeyPress,
          preventsDefault = preventsDefault,
        ),
      )

    def onUp(
        key: String,
        name: String,
        ctrl: Maybe[Boolean] = false.some,
        shift: Maybe[Boolean] = false.some,
        preventsDefault: Boolean = true,
    )(action: KeyboardEvent => Unit): this.type =
      on(
        KeyMap.Key(
          keys = Set(key),
          name = name,
          action = action,
          ctrl = ctrl,
          shift = shift,
          on = On.KeyUp,
          preventsDefault = preventsDefault,
        ),
      )

  }
  object KeyMap {

    final case class Key(
        keys: Set[String],
        name: String,
        action: KeyboardEvent => Unit,
        ctrl: Maybe[Boolean],
        shift: Maybe[Boolean],
        preventsDefault: Boolean,
        on: On,
    ) {

      def matches(e: KeyboardEvent): Boolean =
        keys.contains(e.key) &&
          ctrl.cata(_ == e.ctrlKey, true) &&
          shift.cata(_ == e.shiftKey, true)

    }

    sealed trait On
    object On {
      case object KeyDown extends On
      case object KeyPress extends On
      case object KeyUp extends On
    }

  }

}
