package klib.webServer.widget.widgets

import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.webServer.widget._

trait forms {

  def submitButton[S](
      label: String = "Submit",
      // TODO (KR) : Decorators
  ): Widget.Submit[Unit, S] =
    Widget.builder.withState.submitAction.elementA { (rh: RaiseHandler[S, CommonRaise.Submit.type]) =>
      button(
        onclick := { (_: Event) =>
          rh.raiseAction(CommonRaise.Submit)
        },
      )(label).render
    }.noValue

}
object forms extends forms
