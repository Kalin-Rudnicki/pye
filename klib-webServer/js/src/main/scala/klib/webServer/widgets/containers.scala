package klib.webServer.widgets

import scala.concurrent.ExecutionContext
import org.scalajs.dom._
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all._
import klib.Implicits._
import klib.fp.types._
import klib.webServer
import klib.webServer._

trait containers {

  final case class FormDecorators(
      saveButtonModifiers: Seq[Modifier] = Seq.empty,
      containerModifiers: Seq[Modifier] = Seq.empty,
  )

  def form[V, S, R](
      wb: Widget.Builder[V, S],
      endpoint: V => WrappedFuture[R],
      submitButtonLabel: String = "Submit",
      decorators: FormDecorators = FormDecorators(),
  )(
      onSuccess: R => Unit,
  )(implicit ec: ExecutionContext, errorHandler: ErrorHandler): Widget.Builder[V, S] =
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
                endpoint(v).onComplete(onSuccess)
              case Dead(errors) =>
                errors.foreach(errorHandler)
            }
          },
        )

        container
      }
    }

  // =====|  |=====

  sealed trait ModalBackgroundClickAction
  object ModalBackgroundClickAction {
    case object Close extends ModalBackgroundClickAction
    final case class Custom(action: Div => Unit) extends ModalBackgroundClickAction
  }

  def makeModal(
      vw: Int,
      vh: Int,
      z: Int = 1,
      clickOffModalAction: Maybe[ModalBackgroundClickAction] = ModalBackgroundClickAction.Close.some,
      modalMods: Seq[Modifier] = Seq.empty,
  )(innerModal: HTMLElement): Div = {
    innerModal.style.width = s"${vw}vw"
    innerModal.style.height = s"${vh}vh"
    innerModal.style.margin = s"${(100 - vh).toFloat / 2}vh ${(100 - vw).toFloat / 2}vw"

    val outerModal =
      div(
        `class` := Page.Standard.names.Modal,
        zIndex := z,
      )(innerModal).render

    val prevOnClick = innerModal.onclick
    innerModal.onclick = e => {
      if (prevOnClick != null)
        prevOnClick(e)
      e.stopPropagation()
    }

    outerModal.onclick = _ =>
      clickOffModalAction match {
        case Some(action) =>
          action match {
            case ModalBackgroundClickAction.Close =>
              outerModal.dispatchEvent(events.closeModalEvent)
            case ModalBackgroundClickAction.Custom(action) =>
              action(outerModal)
          }
        case None =>
      }

    outerModal.addEventListener(
      "close-modal",
      { (e: Event) =>
        e.stopPropagation()
        document.body.removeChild(outerModal)
      },
    )

    outerModal
  }

  def displayModal(
      vw: Int,
      vh: Int,
      z: Int = 1,
      clickOffModalAction: Maybe[ModalBackgroundClickAction] = ModalBackgroundClickAction.Close.some,
      modalMods: Seq[Modifier] = Seq.empty,
  )(innerModal: HTMLElement): Unit =
    document.body.appendChild(
      makeModal(
        vw = vw,
        vh = vh,
        z = z,
        clickOffModalAction = clickOffModalAction,
        modalMods = modalMods,
      )(innerModal),
    )

}
object containers extends containers
