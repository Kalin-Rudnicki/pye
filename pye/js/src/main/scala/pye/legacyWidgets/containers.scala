package pye.legacyWidgets

import org.scalajs.dom._
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import pye.{Widget => _, _}

trait containers {

  // =====|  |=====

  sealed trait ModalBackgroundClickAction
  object ModalBackgroundClickAction {
    case object Close extends ModalBackgroundClickAction
    final case class Custom(action: Div => Unit) extends ModalBackgroundClickAction
  }

  @deprecated(message = "Use new Widget", since = "3.0.0")
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
        `class` := Page.names.Modal,
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
              // outerModal.dispatchEvent(events.closeModalEvent)
              // TODO (KR) :
              ???
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

  @deprecated(message = "Use new Widget", since = "3.0.0")
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
