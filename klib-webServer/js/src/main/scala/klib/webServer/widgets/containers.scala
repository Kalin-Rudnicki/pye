package klib.webServer.widgets

import scala.concurrent.ExecutionContext

import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all._

import klib.fp.types._
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
          { (e: Event) =>
            e.stopPropagation()
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

  def displayModal(
      vw: Int,
      vh: Int,
      z: Int = 1,
      // TODO (KR) : Option for clicking off modal to close it
  )(node: HTMLElement): Unit = {
    node.style.width = s"${vw}vw"
    node.style.height = s"${vh}vh"
    node.style.margin = s"${vh.toFloat / 2}vh ${vw.toFloat / 2}vw"

    val modal =
      div(
        `class` := Page.Standard.names.Modal,
        zIndex := z,
      )(node).render

    modal.addEventListener(
      "close-modal",
      { (e: Event) =>
        e.stopPropagation()
        document.body.removeChild(modal)
      },
    )

    document.body.appendChild(modal)
  }

}
object containers extends containers
