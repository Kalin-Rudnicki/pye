package klib.webServer.widget.widgets

import org.scalajs.dom
import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._
import klib.webServer._
import klib.webServer.CSS.Implicits._
import klib.webServer.widget._
import klib.webServer.widget.widgets.modifiers._

trait inputs {

  sealed trait UpdateOn
  object UpdateOn {
    final case class KeyPress(timeout: Maybe[Int]) extends UpdateOn
    case object Blur extends UpdateOn
  }

  private def genInputW[V: DecodeString](
      inputTag: ConcreteHtmlTag[dom.html.Input],
      filterSubmit: KeyboardEvent => Boolean,
  )(
      updateOn: UpdateOn,
      decorators: Seq[Modifier],
  ): Widget.Submit[Maybe[V], String] =
    Widget.builder
      .withState[String]
      .submitAction
      .elementSA { (rh, s) =>
        var savedTimeout: Maybe[Int] = None
        val _input = inputTag(decorators).render

        // TODO (KR) : Make sure state is updated before re-rendering
        def updateState(): Unit =
          rh.raise(Raise.UpdateState[String](_ => _input.value, reRender = false))

        _input.value = s
        _input.onkeypress = { e =>
          if (filterSubmit(e)) {
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

  def inputW[V: DecodeString](
      updateOn: UpdateOn = UpdateOn.Blur,
      decorators: Seq[Modifier] = Seq.empty,
  ): Widget.Submit[Maybe[V], String] =
    genInputW[V](
      inputTag = input,
      filterSubmit = e => e.keyCode == KeyMap.KeyCode.Enter.keyCode,
    )(
      updateOn = updateOn,
      decorators = Seq[Modifier](
        KlwsS.`klws:input`,
      ) ++ decorators,
    )

  def textAreaW[V: DecodeString](
      updateOn: UpdateOn = UpdateOn.Blur,
      decorators: Seq[Modifier] = Seq.empty,
  ): Widget.Submit[Maybe[V], String] =
    genInputW[V](
      inputTag = textarea.asInstanceOf,
      filterSubmit = e => e.keyCode == KeyMap.KeyCode.Enter.keyCode && e.ctrlKey,
    )(
      updateOn = updateOn,
      decorators = Seq[Modifier](
        KlwsS.`klws:text-area`,
      ) ++ decorators,
    )

  // ---  ---

  def radioGroup[T](
      options: Array[(String, T)],
      allowUnset: Boolean = false,
      decorators: Seq[Modifier] = Seq.empty,
      elementDecorators: Seq[Modifier] = Seq.empty,
  ): Widget[Maybe[T], Maybe[T], Nothing] = {
    val actualDecorators = Seq[Modifier](KlwsS.`klws:radio-group`) ++ decorators

    Widget.builder
      .withState[Maybe[T]]
      .withAction[Nothing]
      .elementSA { (rh, s) =>
        val selectedIndex: Int = s.cata(s => options.indexWhere(_._2 == s), -1)

        val optionNodes =
          options.zipWithIndex.map {
            case ((_label, t), i) =>
              span(
                KlwsS.`klws:radio-group`
                  .e(_.option)
                  .--?(
                    (i == selectedIndex) -> KlwsS.`klws:radio-group`.option.selected,
                    (i == 0) -> KlwsS.`klws:radio-group`.option.first,
                    (i == options.length - 1) -> KlwsS.`klws:radio-group`.option.last,
                  ),
                onclick := { (e: MouseEvent) =>
                  if (e.ctrlKey) {
                    if (allowUnset)
                      rh.raise(Raise.UpdateState[Maybe[T]](_ => None))
                  } else
                    rh.raise(Raise.UpdateState[Maybe[T]](_ => t.some))
                },
              )(_label)(elementDecorators)
          }

        span(optionNodes)(actualDecorators).render
      }
      .withValue(_.pure[?])
  }

}
object inputs extends inputs
