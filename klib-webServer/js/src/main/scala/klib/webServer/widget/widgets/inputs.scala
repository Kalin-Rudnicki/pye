package klib.webServer.widget.widgets

import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._
import klib.webServer
import klib.webServer._
import klib.webServer.widget._
import klib.webServer.widget.widgets

trait inputs {

  sealed trait UpdateOn
  object UpdateOn {
    final case class KeyPress(timeout: Maybe[Int]) extends UpdateOn
    case object Blur extends UpdateOn
  }

  def inputW[V: DecodeString](
      updateOn: UpdateOn = UpdateOn.Blur,
      // TODO (KR) : Decorators
  ): Widget.StdForm[Maybe[V], String] =
    Widget.builder
      .withState[String]
      .submitAction
      .elementSA { (rh, s) =>
        var savedTimeout: Maybe[Int] = None
        val _input = input.render

        def updateState(): Unit =
          rh.raise(Raise.UpdateState(_ => _input.value, force = false))

        _input.value = s
        _input.onkeypress = { e =>
          if (e.keyCode == KeyMap.KeyCode.Enter.keyCode) {
            e.preventDefault()
            rh.raiseAction(CommonRaise.Submit)
          } else
            updateOn match {
              case UpdateOn.KeyPress(timeout) =>
                timeout match {
                  case Some(timeout) =>
                    savedTimeout = window
                      .setTimeout(
                        () => {
                          savedTimeout = None
                          updateState()
                        },
                        timeout,
                      )
                      .some
                  case None =>
                    updateState()
                }
              case UpdateOn.Blur =>
              // Do Nothing
            }
        }
        updateOn match {
          case UpdateOn.Blur =>
            _input.onblur = _ => updateState()
          case UpdateOn.KeyPress(_) =>
          // Do Nothing
        }

        _input
      }
      .withValue(_.ensure(_.nonEmpty).map(implicitly[DecodeString[V]].decode).traverse)

}
object inputs extends inputs
