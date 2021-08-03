package klib.webServer.widget.widgets

import scala.concurrent.ExecutionContext

import klib.Implicits._
import klib.fp.types._
import klib.webServer._
import klib.webServer.widget._

trait Implicits {

  implicit class MaybeWidgetOps[V, S, A](widget: Widget[Maybe[V], S, A]) {

    def required(): Widget[V, S, A] =
      Widget[V, S, A](
        elementF = widget.elementF,
        valueF = widget.valueF(_).flatMap {
          case Some(v) =>
            v.pure[?]
          case None =>
            // TODO (KR) : Name and stuff
            ?.dead(Message("Missing required value"))
        },
      )

  }

  implicit class NoActionWidgetOps[V, S](widget: Widget.NoAction[V, S]) {

    def renderNoAction(initialState: S)(implicit ec: ExecutionContext): Widget.ElementT =
      widget.render { _ => Nil.pure[WrappedFuture] }(initialState)

  }

}
object Implicits extends Implicits
