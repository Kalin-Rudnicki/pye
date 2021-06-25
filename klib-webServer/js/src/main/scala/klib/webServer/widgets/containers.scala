package klib.webServer.widgets

import scala.concurrent.ExecutionContext

import klib.fp.types.{Alive, Dead}
import org.scalajs.dom.Event
import scalatags.JsDom.all._
import klib.webServer._

trait containers {

  final case class FormDecorators(
      saveButtonModifiers: Seq[Modifier] = Seq.empty,
      containerModifiers: Seq[Modifier] = Seq.empty,
  )

  def form[V, S, R](
      wb: Widget.Builder[V, S],
      endpoint: V => HttpResponse[R],
      errorHandler: Throwable => Unit,
      submitButtonLabel: String = "Submit",
      decorators: FormDecorators = FormDecorators(),
  )(
      onSuccess: R => Unit,
  )(implicit ec: ExecutionContext): Widget.Builder[V, S] =
    Widget.Builder[V, S] { s =>
      val w = wb(s)
      Widget(w.value) {
        val container =
          div(`class` := "kws:form-container")(
            w.render(),
            br, {
              val btn = button(submitButtonLabel, `class` := "kws:form-button")(decorators.saveButtonModifiers).render
              btn.onclick = _ => btn.dispatchEvent(events.submitEvent)
              btn
            },
          ).render

        container.addEventListener(
          "submit",
          { (_: Event) =>
            w.value match {
              case Alive(v) =>
                endpoint(v).onComplete(errorHandler)(onSuccess)
              case Dead(errors) =>
                errors.foreach(errorHandler)
            }
          },
        )

        container
      }
    }

}
object containers extends containers
