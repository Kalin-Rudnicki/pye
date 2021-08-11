package pye.widgets

import org.scalajs.dom._
import scalatags.JsDom.all._

import pye._
import pye.Implicits._
import pye.widgets.modifiers._

trait forms {

  def submitButton[S](
      label: String = "Submit",
      decorators: Seq[Modifier] = Seq.empty,
  ): Widget.Submit[Unit, S] =
    Widget.builder.withState.submitAction.rElement { (rh: RaiseHandler[S, CommonRaise.Submit.type]) =>
      button(PyeS.`pye:form-button`)(
        onclick := { (_: Event) =>
          rh.raiseAction(CommonRaise.Submit)
        },
      )(label)(decorators).render
    }.noValue

}
object forms extends forms
