package pye

import scala.scalajs.js.URIUtils

import org.scalajs.dom._
import org.scalajs.dom.{html => H}
import scalatags.JsDom.all._

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

  override def toString: String =
    titleF match {
      case Right(_)    => "Page(<titleF>)"
      case Left(title) => s"Page($title)"
    }

  // =====|  |=====

  private final def renderAnd(and: String => IO[Unit]): AsyncIO[Unit] =
    for {
      env <- getEnv
      stateVar = Var(env)

      title = titleF match {
        case Right(f) => f(env)
        case Left(s)  => s
      }

      (appliedWidget, rh) =
        Pointer
          .withSelf[(AppliedWidget[Unit], RaiseHandler[Env, A])] { ptr =>
            val handleSOU: Raise.StandardOrUpdate[Env] => AsyncIO[RaiseHandler.ReRender] = {
              case standard: Raise.Standard =>
                standard match {
                  case msg: Raise.DisplayMessage =>
                    AsyncIO { displayMessage(msg); RaiseHandler.ReRender() }
                  case history: Raise.History =>
                    {
                      history match {
                        case Raise.History.Push(page)    => page().push
                        case Raise.History.Replace(page) => page().replace
                        case Raise.History.Go(delta)     => AsyncIO { window.history.go(delta) }
                      }
                    }.map { _ => RaiseHandler.ReRender.PageChange }
                  case Raise.RefreshPage =>
                    AsyncIO { RaiseHandler.ReRender(ptr.value._1) }
                  case raw: Raise.Raw =>
                    raw.action.map { _ => RaiseHandler.ReRender() }
                }
              case update: Raise.UpdateState[Env] =>
                for {
                  _ <- { stateVar.value = update.update(stateVar.value) }.toAsyncIO
                  _ <- PyeLogger.log.debug(s"env: ${stateVar.value}", "env").toAsyncIO
                  rr <- AsyncIO {
                    update.reRender ?
                      RaiseHandler.ReRender(ptr.value._1) |
                      update.childReRenders
                  }
                  _ <- titleF match {
                    case Right(f) => AsyncIO { window.document.title = f(stateVar.value) }
                    case Left(_)  => AsyncIO {}
                  }
                } yield rr
            }
            val rh: RaiseHandler[Env, A] = {
              case sou: Raise.StandardOrUpdate[Env] =>
                handleSOU(sou)
              case action: Raise.Action[A] =>
                for {
                  raises <- handleA(action.action)
                  rrs <- AsyncIO.runSequentially(raises.map(handleSOU))
                } yield RaiseHandler.ReRender.merge(rrs)
            }

            // TODO (KR) : Fit modals in there somewhere

            Pointer {
              (
                widget
                  .wrapped { elems => body(elems).render }
                  .convert(rh, () => stateVar.value),
                rh,
              )
            }
          }
          .value

      bindableKeyMap = keyMap.withRaiseHandler(rh)
      _ <- {
        for {
          bodyElems <- appliedWidget.reRender
          _ <- IO { bindableKeyMap.bindTo(window) }
          _ <- IO { window.document.title = title }
          _ <- IO { window.document.body = bodyElems.head.asInstanceOf[H.Body] }
          _ <- and(title)
        } yield ()
      }.toAsyncIO
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

}

object Page {

  object names {

    val Page = "page"

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
      val pathStr =
        ("pages" :: paths.toList)
          .map(URIUtils.encodeURIComponent)
          .mkString("/", "/", "")

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
                            rh.raise(Raise.History.push(page()))
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
            .mapValue { _ => }
            .wrapped { elems =>
              div(PyeS.`pye:nav-bar`)(elems).render
            },
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
            Seq[Modifier](
              overflowY.auto,
              flexShrink := 0,
            ),
          )
        val pageRight =
          map2(
            Page.names.PageRight,
            _pageRight,
            Seq[Modifier](
              overflowY.auto,
              flexShrink := 0,
            ),
          )

        val pageCenterTop =
          map2(
            Page.names.PageCenterTop,
            _pageCenterTop,
            Seq[Modifier](
              flexShrink := 0,
            ),
          )
        val pageCenterBottom =
          map2(
            Page.names.PageCenterBottom,
            _pageCenterBottom,
            Seq[Modifier](
              flexShrink := 0,
            ),
          )

        val pageCenterMiddle =
          mapSection(
            Page.names.PageCenterMiddle,
            None,
            _pageCenterMiddle._1,
            Seq[Modifier](
              flexGrow := 1,
              overflowY.auto,
              _pageCenterMiddle._2,
            ),
          )

        // ---  ---

        type PageWidget[V] = Widget[V, Env, A]

        val pageMessages: Widget[Unit, Env, A] =
          Widget.builder.element {
            div(
              id := Page.names.PageMessages,
              flexShrink := 0,
            ).render
          }

        val pageCenter: PageWidget[Unit] =
          NonEmptyList
            .nelJoin[PageWidget[Unit]](pageCenterTop, pageMessages.some)(pageCenterMiddle)(pageCenterBottom)
            .traverse
            .mapValue { _ => }
            .wrapped { elems =>
              div(
                id := Page.names.PageCenter,
                display.flex,
                flexDirection.column,
                flexGrow := 1,
              )(elems)(_pageCenterModifier).render
            }

        val pageMiddle: PageWidget[Unit] =
          NonEmptyList
            .nelJoin[PageWidget[Unit]](pageLeft)(pageCenter)(pageRight)
            .traverse
            .mapValue { _ => }
            .wrapped { elems =>
              div(
                id := Page.names.PageMiddle,
                display.flex,
                height := {
                  List(
                    _pageTop.map(_._1),
                    _pageBottom.map(_._1),
                  ).flatMap(_.toOption).toNel match {
                    case Some(edges) =>
                      ("100vh" :: edges.toList).mkString("calc(", " - ", ")")
                    case None =>
                      "100vh"
                  }
                },
              )(elems)(_pageMiddleModifier).render
            }

        val page: PageWidget[Unit] =
          NonEmptyList
            .nelJoin[PageWidget[Unit]](pageTop)(pageMiddle)(pageBottom)
            .traverse
            .mapValue { _ => }
            .wrapped { elems =>
              div(
                id := Page.names.Page,
                height := "100vh",
              )(elems)(_pageModifier).render
            }

        page
      }

    }
  }

}
