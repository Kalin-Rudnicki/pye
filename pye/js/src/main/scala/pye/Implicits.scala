package pye

import org.scalajs.dom.Element
import scalatags.JsDom.all._

import klib.fp.types._

trait Implicits {

  implicit class MaybeWidgetOps[V, S, A](widget: Widget[Maybe[V], S, A]) {

    def required(message: String = "Missing required value"): Widget[V, S, A] =
      widget.flatMapValue(_.toEA(Message(message)))

  }

  implicit class AsyncIOOps[T](asyncIO: AsyncIO[T]) {

    def runAndShowErrors(onComplete: T => Unit = (_: T) => ()): Unit =
      asyncIO.runASyncGlobal {
        case Alive(res) =>
          onComplete(res)
        case Dead(errors) =>
          errors.map(Raise.DisplayMessage.fromThrowable).foreach(displayMessage)
      }

  }

  implicit class HtmlTagOps(tag: ConcreteHtmlTag[_ <: Element]) {

    def asWidget[Env, A]: Widget[Unit, Env, A] =
      Widget.builder.element(tag.render)

  }

}
object Implicits extends Implicits with CSS.Implicits with Widget.Implicits
