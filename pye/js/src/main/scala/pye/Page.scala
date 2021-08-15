package pye

import scala.scalajs.js.URIUtils

import org.scalajs.dom._
import org.scalajs.dom.{html => H}
import scalatags.JsDom.all._
import scalatags.JsDom.tags2

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils._
import klib.utils._
import pye.Implicits._
import pye.widgets.modifiers.PyeS

sealed trait Page { page =>
  type Env
  type A
  val url: String
  val getEnv: AsyncIO[Env]
  val titleF: String \/ (Env => String)
  val widget: Widget[Unit, Env, A]
  val keyMap: KeyMap[Env, A]
  val handleA: A => AsyncIO[List[Raise.StandardOrUpdate[Env]]]

  // =====|  |=====

  private final def renderAnd(and: String => IO[Unit]): AsyncIO[Unit] =
    for {
      _ <- AsyncIO { console.log("2.1") }
      env <- getEnv
      _ <- AsyncIO { console.log("2.2") }
      stateVar = Var(env)

      _ <- AsyncIO { console.log("2.3") }
      title = titleF match {
        case Right(f) => f(env)
        case Left(s)  => s
      }

      _ <- AsyncIO { console.log("2.4") }
      (appliedWidget, rh) =
        Pointer
          .withSelf[(AppliedWidget[Unit], RaiseHandler[Env, A])] { ptr =>
            console.log("2.4.1")
            val handleSOU: Raise.StandardOrUpdate[Env] => AsyncIO[Unit] = {
              case standard: Raise.Standard =>
                standard match {
                  case msg: Raise.DisplayMessage =>
                    AsyncIO { displayMessage(msg) }
                  case history: Raise.History =>
                    history match {
                      case Raise.History.Push(page)    => page.push
                      case Raise.History.Replace(page) => page.replace
                      case Raise.History.Go(delta)     => AsyncIO { window.history.go(delta) }
                    }
                  case Raise.RefreshPage =>
                    AsyncIO.wrapIO { ptr.value._1.reRender }.map { _ => }
                  case raw: Raise.Raw =>
                    raw.action
                }
              case update: Raise.UpdateState[Env] =>
                for {
                  _ <- AsyncIO.wrapIO { stateVar.value = update.update(stateVar.value) }
                  _ <- AsyncIO.wrapIO { update.reRender.maybe(ptr.value._1.reRender).traverse }
                  _ <- titleF match {
                    case Right(f) => AsyncIO { window.document.title = f(stateVar.value) }
                    case Left(_)  => AsyncIO {}
                  }
                } yield ()
            }
            console.log("2.4.2")
            val rh: RaiseHandler[Env, A] = {
              case sou: Raise.StandardOrUpdate[Env] =>
                handleSOU(sou)
              case action: Raise.Action[A] =>
                for {
                  raises <- handleA(action.action)
                  _ <- AsyncIO.runSequentially(raises.map(handleSOU))
                } yield ()
            }

            console.log("2.4.3")
            // TODO (KR) : Fit modals in there somewhere
            Pointer {
              console.log(s"2.4.3.1: $widget")
              val p1 = widget.wrapped { elems => body(elems).render }
              console.log(s"2.4.3.2: $p1")
              val p2 = p1.convert(rh, () => stateVar.value)
              console.log(s"2.4.3.3: $p2")

              (
                p2,
                rh,
              )
            }
          }
          .value

      _ <- AsyncIO { console.log("2.5") }
      bindableKeyMap = keyMap.withRaiseHandler(rh)
      _ <- AsyncIO { console.log("2.6") }
      _ <- AsyncIO.wrapIO {
        for {
          bodyElems <- appliedWidget.reRender
          _ <- IO { bindableKeyMap.bindTo(window) }
          _ <- IO { window.document.title = title }
          _ <- IO { window.document.body = bodyElems.head.asInstanceOf[H.Body] }
          _ <- and(title)
        } yield ()

      }
      _ <- AsyncIO { console.log("2.7") }
    } yield ()

  private[pye] final def push: AsyncIO[Unit] =
    renderAnd { title =>
      IO { window.history.pushState(null, title, url) }
    }

  private[pye] final def replace: AsyncIO[Unit] =
    renderAnd { title =>
      IO { window.history.replaceState(null, title, url) }
    }

  private[pye] final def replaceNoTrace: AsyncIO[Unit] =
    renderAnd { _ => IO {} }

  // =====|  |=====

  object history {
    def push: Raise.History.Push = Raise.History.Push(page)
    def replace: Raise.History.Replace = Raise.History.Replace(page)
  }

}

object Page {

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

  // =====|  |=====

  sealed trait NavBarItem[+A]
  object NavBarItem {

    final case class Button[+A](
        action: NavBarAction[A],
        modifier: Modifier = Seq.empty[Modifier],
    ) extends NavBarItem[A]
    object Button {
      def build[A](action: NavBarAction[A], modifiers: Modifier*): Button[A] =
        Button(action, modifiers)
    }

  }

  sealed trait NavBarAction[+A]
  object NavBarAction {
    final case class PushPage(page: () => Page) extends NavBarAction[Nothing]
    final case class Custom[+A](onClick: MouseEvent => List[Raise[Nothing, A]]) extends NavBarAction[A]
  }

  // =====|  |=====

  object builder {

    def apply(
        paths: String*,
    )(
        paramF: RouteMatcher.Params => RouteMatcher.Params = identity,
    ): Builder1 = {
      console.log("1.1")

      val pathStr =
        ("pages" :: paths.toList)
          .map(URIUtils.encodeURIComponent)
          .mkString("/", "/", "")

      console.log("1.2")

      val params = paramF(RouteMatcher.Params.empty)
      val paramStr =
        if (params.paramMap.isEmpty)
          ""
        else
          params.paramMap.toList
            .map {
              case (key, value) =>
                s"${URIUtils.encodeURIComponent(key)}=${URIUtils.encodeURIComponent(value)}"
            }
            .mkString("?", "&", "")

      console.log("1.3")

      new Builder1(
        url = pathStr + paramStr,
      )
    }

    final class Builder1 private[builder] (
        url: String,
    ) {

      def constEnv[Env](envF: => Env): Builder2[Env] =
        env(envF.pure[AsyncIO])

      def env[Env](getEnv: AsyncIO[Env]): Builder2[Env] =
        new Builder2[Env](
          url = url,
          getEnv = getEnv,
        )

      def noEnv: Builder2[Unit] =
        env(().pure[AsyncIO])

    }

    final class Builder2[Env] private[builder] (
        url: String,
        getEnv: AsyncIO[Env],
    ) {

      def constTitle(title: String): Builder3[Env] =
        new Builder3[Env](
          url = url,
          getEnv = getEnv,
          titleF = title.left,
        )

      def envTitle(titleF: Env => String): Builder3[Env] =
        new Builder3[Env](
          url = url,
          getEnv = getEnv,
          titleF = titleF.right,
        )

    }

    final class Builder3[Env] private[builder] (
        url: String,
        getEnv: AsyncIO[Env],
        titleF: String \/ (Env => String),
    ) {

      def body[A](widget: Widget[Unit, Env, A]): Builder4[Env, A] =
        new Builder4[Env, A](
          url = url,
          getEnv = getEnv,
          titleF = titleF,
          widget = widget,
        )

      def standardBody[A](makeStandard: StandardBuilder1[Env] => StandardBuilder2[Env, A]): Builder4[Env, A] =
        body(makeStandard(new StandardBuilder1[Env]).toWidget)

    }

    final class Builder4[Env, +A] private[builder] (
        url: String,
        getEnv: AsyncIO[Env],
        titleF: String \/ (Env => String),
        widget: Widget[Unit, Env, A],
    ) {

      def keyMap[A2 >: A](keyMap: KeyMap[Env, A2]): Builder5[Env, A2] =
        new Builder5[Env, A2](
          url = url,
          getEnv = getEnv,
          titleF = titleF,
          widget = widget,
          keyMap = keyMap,
        )

      def noKeyMap: Builder5[Env, A] =
        keyMap[A](KeyMap.empty)

    }

    final class Builder5[Env, +A] private[builder] (
        url: String,
        getEnv: AsyncIO[Env],
        titleF: String \/ (Env => String),
        widget: Widget[Unit, Env, A],
        keyMap: KeyMap[Env, A],
    ) {
      builder =>
      private type _Env = Env

      def handleA[A2 >: A](_handleA: A2 => AsyncIO[List[Raise.StandardOrUpdate[Env]]]): Page =
        new Page {
          override final type Env = builder._Env
          override type A = A2
          override final val url: String = builder.url
          override final val getEnv: AsyncIO[Env] = builder.getEnv
          override final val titleF: \/[String, Env => String] = builder.titleF
          override final val widget: Widget[Unit, Env, A] = builder.widget
          override final val keyMap: KeyMap[Env, A] = builder.keyMap
          override val handleA: A => AsyncIO[List[Raise.StandardOrUpdate[_Env]]] = _handleA
        }

      def ignoreA: Page = handleA[A] { _ => Nil.pure[AsyncIO] }

    }

    // ---  ---

    final class StandardBuilder1[Env] private[Page] {

      def pageCenterMiddle[A](
          widget: Widget[Unit, Env, A],
          modifier: Modifier = Seq.empty[Modifier],
      ): StandardBuilder2[Env, A] =
        new StandardBuilder2[Env, A](
          _pageCenterMiddle = (widget, modifier),
          _pageTop = None,
          _pageBottom = None,
          _pageLeft = None,
          _pageRight = None,
          _pageCenterTop = None,
          _pageCenterBottom = None,
          _pageCenterModifier = Seq.empty[Modifier],
          _pageMiddleModifier = Seq.empty[Modifier],
          _pageModifier = Seq.empty[Modifier],
        )

    }

    final class StandardBuilder2[Env, +A] private[Page] (
        // Required Widget
        _pageCenterMiddle: (Widget[Unit, Env, A], Modifier),
        // Optional Widgets
        _pageTop: Maybe[(String, Widget[Unit, Env, A], Modifier)],
        _pageBottom: Maybe[(String, Widget[Unit, Env, A], Modifier)],
        _pageLeft: Maybe[(Widget[Unit, Env, A], Modifier)],
        _pageRight: Maybe[(Widget[Unit, Env, A], Modifier)],
        _pageCenterTop: Maybe[(Widget[Unit, Env, A], Modifier)],
        _pageCenterBottom: Maybe[(Widget[Unit, Env, A], Modifier)],
        // Section Modifiers
        _pageCenterModifier: Modifier,
        _pageMiddleModifier: Modifier,
        _pageModifier: Modifier,
        // TODO (KR) : Modals
    ) {

      private def copy[A2 >: A](
          _pageCenterMiddle: (Widget[Unit, Env, A2], Modifier) = this._pageCenterMiddle,
          _pageTop: Maybe[(String, Widget[Unit, Env, A2], Modifier)] = this._pageTop,
          _pageBottom: Maybe[(String, Widget[Unit, Env, A2], Modifier)] = this._pageBottom,
          _pageLeft: Maybe[(Widget[Unit, Env, A2], Modifier)] = this._pageLeft,
          _pageRight: Maybe[(Widget[Unit, Env, A2], Modifier)] = this._pageRight,
          _pageCenterTop: Maybe[(Widget[Unit, Env, A2], Modifier)] = this._pageCenterTop,
          _pageCenterBottom: Maybe[(Widget[Unit, Env, A2], Modifier)] = this._pageCenterBottom,
          _pageCenterModifier: Modifier = this._pageCenterModifier,
          _pageMiddleModifier: Modifier = this._pageMiddleModifier,
          _pageModifier: Modifier = this._pageModifier,
      ): StandardBuilder2[Env, A2] =
        new StandardBuilder2[Env, A2](
          _pageCenterMiddle = _pageCenterMiddle,
          _pageTop = _pageTop,
          _pageBottom = _pageBottom,
          _pageLeft = _pageLeft,
          _pageRight = _pageRight,
          _pageCenterTop = _pageCenterTop,
          _pageCenterBottom = _pageCenterBottom,
          _pageCenterModifier = _pageCenterModifier,
          _pageMiddleModifier = _pageMiddleModifier,
          _pageModifier = _pageModifier,
        )

      def noPageTop: StandardBuilder2[Env, A] = copy(_pageTop = None)
      def pageTop[A2 >: A](
          height: String,
          widget: Widget[Unit, Env, A2],
          modifier: Modifier = Seq.empty[Modifier],
      ): StandardBuilder2[Env, A2] =
        copy(_pageTop = (height, widget, modifier).some)
      def navBarTop[A2 >: A]( // TODO (KR) : Env (?)
          height: String,
          leftItems: List[NavBarItem[A2]],
          rightItems: List[NavBarItem[A2]],
          modifier: Modifier = Seq.empty[Modifier],
      ): StandardBuilder2[Env, A2] = {
        type WidgetT[V] = Widget[V, Env, A2]

        def convertItems(items: List[NavBarItem[A2]]): WidgetT[Unit] = {
          def convertItem(item: NavBarItem[A2]): WidgetT[Unit] =
            item match {
              case NavBarItem.Button(action, modifier) =>
                Widget.builder
                  .withState[Env]
                  .withAction[A2]
                  .rElement { rh =>
                    div(PyeS.`pye:nav-bar`.item)(
                      onclick := { (e: MouseEvent) =>
                        action match {
                          case NavBarAction.PushPage(page) =>
                            rh.raise(page().history.push)
                          case NavBarAction.Custom(onClick) =>
                            rh.raises(onClick(e))
                        }
                      },
                    )(modifier).render
                  }
                  .noValue
            }

          items
            .map(convertItem)
            .traverse
            .mapValue { _ => }
            .wrapped { elems =>
              div(PyeS.`pye:nav-bar`.e(_.section).m(_.wrap))(elems).render
            }
        }

        pageTop(
          height,
          ado[WidgetT]
            .join(
              convertItems(leftItems),
              Widget.builder.element(div(PyeS.`pye:nav-bar`.e(_.section).m(_.expand)).render),
              convertItems(rightItems),
            )
            .mapValue { _ => },
          modifier,
        )
      }

      def noPageBottom: StandardBuilder2[Env, A] = copy(_pageBottom = None)
      def pageBottom[A2 >: A](
          height: String,
          widget: Widget[Unit, Env, A2],
          modifier: Modifier = Seq.empty[Modifier],
      ): StandardBuilder2[Env, A2] =
        copy(_pageBottom = (height, widget, modifier).some)

      def noPageLeft: StandardBuilder2[Env, A] = copy(_pageLeft = None)
      def pageLeft[A2 >: A](
          widget: Widget[Unit, Env, A2],
          modifier: Modifier = Seq.empty[Modifier],
      ): StandardBuilder2[Env, A2] =
        copy(_pageLeft = (widget, modifier).some)

      def noPageRight: StandardBuilder2[Env, A] = copy(_pageRight = None)
      def pageRight[A2 >: A](
          widget: Widget[Unit, Env, A2],
          modifier: Modifier = Seq.empty[Modifier],
      ): StandardBuilder2[Env, A2] =
        copy(_pageRight = (widget, modifier).some)

      def noPageCenterTop: StandardBuilder2[Env, A] = copy(_pageCenterTop = None)
      def pageCenterTop[A2 >: A](
          widget: Widget[Unit, Env, A2],
          modifier: Modifier = Seq.empty[Modifier],
      ): StandardBuilder2[Env, A2] =
        copy(_pageCenterTop = (widget, modifier).some)

      def noPageCenterBottom: StandardBuilder2[Env, A] = copy(_pageCenterBottom = None)
      def pageCenterBottom[A2 >: A](
          widget: Widget[Unit, Env, A2],
          modifier: Modifier = Seq.empty[Modifier],
      ): StandardBuilder2[Env, A2] =
        copy(_pageCenterBottom = (widget, modifier).some)

      // ---  ---

      def modifyPageCenter(mods: Modifier*): StandardBuilder2[Env, A] =
        copy(_pageCenterModifier = Seq[Modifier](this._pageCenterModifier, mods))
      def modifyPageMiddle(mods: Modifier*): StandardBuilder2[Env, A] =
        copy(_pageMiddleModifier = Seq[Modifier](this._pageMiddleModifier, mods))
      def modifyPage(mods: Modifier*): StandardBuilder2[Env, A] =
        copy(_pageModifier = Seq[Modifier](this._pageModifier, mods))

      // TODO (KR) : Modals

      // =====|  |=====

      private[Page] def toWidget: Widget[Unit, Env, A] = {
        def mapSection(
            _id: String,
            _height: Maybe[String],
            widget: Widget[Unit, Env, A],
            modifier: Modifier,
        ): Widget[Unit, Env, A] =
          widget.wrapped { elems =>
            div(id := _id, _height.map(height := _).toOption)(elems)(modifier).render
          }

        def map1(
            _id: String,
            m: Maybe[(String, Widget[Unit, Env, A], Modifier)],
            extraModifier: Modifier,
        ): Maybe[Widget[Unit, Env, A]] =
          m.map { case (height, widget, modifier) => mapSection(_id, height.some, widget, Seq(modifier, extraModifier)) }

        def map2(
            _id: String,
            m: Maybe[(Widget[Unit, Env, A], Modifier)],
            extraModifier: Modifier,
        ): Maybe[Widget[Unit, Env, A]] =
          m.map { case (widget, modifier) => mapSection(_id, None, widget, Seq(modifier, extraModifier)) }

        // ---  ---

        val pageTop =
          map1(
            Page.names.PageTop,
            _pageTop,
            Seq.empty[Modifier],
          ) // TODO (KR) :
        val pageBottom =
          map1(
            Page.names.PageBottom,
            _pageBottom,
            Seq.empty[Modifier],
          ) // TODO (KR) :

        val pageLeft =
          map2(
            Page.names.PageLeft,
            _pageLeft,
            Seq.empty[Modifier],
          ) // TODO (KR) :
        val pageRight =
          map2(
            Page.names.PageRight,
            _pageRight,
            Seq.empty[Modifier],
          ) // TODO (KR) :

        val pageCenterTop =
          map2(
            Page.names.PageCenterTop,
            _pageCenterTop,
            Seq.empty[Modifier],
          ) // TODO (KR) :
        val pageCenterBottom =
          map2(
            Page.names.PageCenterBottom,
            _pageCenterBottom,
            Seq.empty[Modifier],
          ) // TODO (KR) :

        val pageCenterMiddle =
          mapSection(
            Page.names.PageCenterMiddle,
            List(
              _pageTop.map(_._1),
              _pageBottom.map(_._1),
            ).flatMap(_.toOption).toNel match {
              case Some(edges) =>
                ("100vh" :: edges.toList).mkString("calc(", " - ", ")").some
              case None =>
                "100vh".some
            },
            _pageCenterMiddle._1,
            Seq[Modifier](
              // TODO (KR) : Style
              _pageCenterMiddle._2,
            ),
          )

        // ---  ---

        type PageWidget[V] = Widget[V, Env, A]

        // TODO (KR) :
        val pageMessages: Widget[Unit, Env, A] =
          Widget.builder.element {
            div(id := Page.names.PageMessages).render
          }

        val pageCenter: PageWidget[Unit] =
          NonEmptyList
            .nelJoin[PageWidget[Unit]](pageCenterTop, pageMessages.some)(pageCenterMiddle)(pageCenterBottom)
            .traverse
            .mapValue { _ => }
            .wrapped { elems =>
              div(id := Page.names.PageCenter)(elems)(_pageCenterModifier).render
            }

        val pageMiddle: PageWidget[Unit] =
          NonEmptyList
            .nelJoin[PageWidget[Unit]](pageLeft)(pageCenter)(pageRight)
            .traverse
            .mapValue { _ => }
            .wrapped { elems =>
              div(id := Page.names.PageMiddle)(elems)(_pageMiddleModifier).render
            }

        val page: PageWidget[Unit] =
          NonEmptyList
            .nelJoin[PageWidget[Unit]](pageTop)(pageMiddle)(pageBottom)
            .traverse
            .mapValue { _ => }
            .wrapped { elems =>
              div(id := Page.names.Page)(elems)(_pageModifier).render
            }

        page
      }

    }
  }

}

/*

     keyMap2
       .onDown(KeyMap.KeyCode.F1)("Display KeyMap") { _ =>
         console.log("Display modal")
         containers.displayModal(
           vw = 50,
           vh = 60,
         ) {
           def section(label: String, keys: List[KeyMap.Key]): Node =
             if (keys.isEmpty)
               ().render
             else
               div(S.`key-map-modal`.e(_.section))(
                 h2(S.centered)(s"--- $label ---"),
                 table(S.`key-map-modal`.e(_.table))(
                   tr(
                     th(width := "45%")("Key(s)"),
                     th(width := "55%")("Action"),
                   ),
                   keys.map { key =>
                     def stringify(kc: KeyMap.KeyCode): String =
                       s"${key.ctrl.getOrElse(false) ? "Ctrl + " | ""}${key.shift.getOrElse(false) ? "Shift + " | ""}${kc.name}"

                     tr(
                       td(
                         key.keys match {
                           case InfiniteSet.Inclusive(explicit) =>
                             explicit.toList.map { kc =>
                               div(S.`key-map-modal`.e(_.text))(stringify(kc))
                             }
                           case InfiniteSet.Exclusive(explicit) =>
                             List[Frag](
                               div(S.`key-map-modal`.e(_.header))("Everything except:"),
                               explicit.toList.map { kc =>
                                 div(S.`key-map-modal`.e(_.text))(stringify(kc))
                               },
                             )
                         },
                       ),
                       td(key.name),
                     )
                   },
                 ),
               ).render

           val node =
             div(S.`std-modal`, tabindex := 0)(
               div(S.`std-modal`.container)(
                 h1(S.centered)("KeyMap"),
                 section("On KeyDown", keyMap2.onDownKeys.reverse),
                 section("On KeyPress", keyMap2.onPressKeys.reverse),
                 section("On KeyUp", keyMap2.onUpKeys.reverse),
               ),
             ).render

           window.setTimeout(() => node.focus(), 50)

           node
         }
       }
 */

/*
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
 */

// REMOVE : ...
/*
final class StandardBuilder1[Env] private[Page] {

def pageCenterMiddle(build: Widget[Unit, Env, PlaceHolderA]): StandardBuilder2[Env] =
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
private[Page] val _pageCenterMiddle: Widget[Unit, Env, PlaceHolderA],
private[Page] val _pageTop: Maybe[Widget[Unit, (String, Env), PlaceHolderA]],
private[Page] val _pageBottom: Maybe[Widget[Unit, (String, Env), PlaceHolderA]],
private[Page] val _pageLeft: Maybe[Widget[Unit, Env, PlaceHolderA]],
private[Page] val _pageRight: Maybe[Widget[Unit, Env, PlaceHolderA]],
private[Page] val _pageCenterTop: Maybe[Widget[Unit, Env, PlaceHolderA]],
private[Page] val _pageCenterBottom: Maybe[Widget[Unit, Env, PlaceHolderA]],
private[Page] val _inBody: List[Widget[Unit, Env, PlaceHolderA]],
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

def standardBody(bodyF: StandardBuilder1[Env] => StandardBuilder2[Env]): Page[Env] = {
import Standard.{names => N}

val sb = bodyF(new StandardBuilder1[Env])

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
 */
