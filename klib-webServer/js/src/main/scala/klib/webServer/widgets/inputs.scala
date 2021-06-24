package klib.webServer.widgets

import org.scalajs.dom
import org.scalajs.dom._
import scalatags.JsDom.{TypedTag, all => JD}
import scalatags.JsDom.all._
import klib.Implicits._
import klib.fp.types._
import org.scalajs.dom.html.Input

trait inputs {

  final case class Decorators(
      labelModifiers: Seq[Modifier] = Seq.empty,
      inputModifiers: Seq[Modifier] = Seq.empty,
      errorsModifiers: Seq[Modifier] = Seq.empty,
      containerModifiers: Seq[Modifier] = Seq.empty,
  )

  // =====|  |=====

  private def submitEvent: Event =
    new Event(
      "submit",
      new org.scalajs.dom.raw.EventInit {
        bubbles = true
      },
    )

  // --- input ---

  private def textWidget(
      _type: String,
      __input: TypedTag[Input],
      filterSubmit: KeyboardEvent => Boolean,
  )(
      label: String,
      id: String,
      decorators: Decorators,
  ): Widget.Builder[Maybe[String], Var[String]] =
    Widget.Builder[Maybe[String], Var[String]] { s =>
      val _label =
        JD.label(JD.id := s"$id-label", `for` := id, `class` := s"kws:${_type} kws:field-label", label)(
          decorators.labelModifiers,
        )
      val _input = __input(JD.id := id, `class` := s"kws:${_type}")(decorators.inputModifiers).render

      val _errors =
        span(JD.id := s"$id-errors", `class` := s"kws:${_type}-errors kws:field-errors")(decorators.errorsModifiers)

      val prevOnKeyPress = _input.onkeypress
      _input.onkeypress = { e =>
        if (prevOnKeyPress != null)
          prevOnKeyPress(e)
        if (!e.defaultPrevented) {
          if (filterSubmit(e)) {
            e.preventDefault()
            s.value = _input.value
            _input.dispatchEvent(submitEvent)
          }
        }
      }

      val prevOnBlur = _input.onblur
      _input.onblur = { e =>
        if (prevOnBlur != null)
          prevOnBlur(e)
        if (!e.defaultPrevented) {
          s.value = _input.value
        }
      }

      _input.value = s.value

      Widget(
        span(JD.id := s"$id-container", `class` := s"kws:${_type}-container kws:field-container")(
          _label,
          _input,
          _errors,
        )(decorators.containerModifiers).render,
        s.value.ensure(_.nonEmpty).pure[?],
      )
    }

  def input(
      label: String,
      id: String,
      decorators: Decorators = Decorators(),
  ): Widget.Builder[Maybe[String], Var[String]] =
    textWidget(
      "input",
      JD.input,
      _.key == "Enter",
    )(
      label,
      id,
      decorators,
    )

  // --- text area ---

  def textArea(
      label: String,
      id: String,
      decorators: Decorators = Decorators(),
  ): Widget.Builder[Maybe[String], Var[String]] =
    textWidget(
      "input",
      JD.textarea.asInstanceOf[TypedTag[Input]],
      _.key == "Enter",
    )(
      label,
      id,
      decorators,
    )

  // =====|  |=====

  def radioGroup[T](
      label: String,
      id: String,
      options: Array[(String, T)],
      onChange: Maybe[T => Unit] = None,
      decorators: Decorators = Decorators(),
  ): Widget.Builder[Maybe[T], Var[Maybe[T]]] =
    Widget.Builder[Maybe[T], Var[Maybe[T]]] { s =>
      object selected {
        private var _value: Maybe[(T, dom.html.Span)] = None

        def value: Maybe[(T, dom.html.Span)] = _value
        def value_=(v: Maybe[(T, dom.html.Span)]): Unit = {
          s.value = v.map(_._1)

          _value.foreach(_._2.classList.remove("kws:radio-group__button--selected"))
          v.foreach(_._2.classList.add("kws:radio-group__button--selected"))
          _value = v
        }
      }

      val optionNodes: Array[dom.html.Span] =
        options.zipWithIndex.map {
          case ((label, t), i) =>
            val n =
              span(
                `class` := List(
                  List(
                    "clickable",
                    "kws:radio-group__button",
                  ),
                  (i == 0).maybe("kws:radio-group__button--first").toList,
                  (i == options.length - 1).maybe("kws:radio-group__button--last").toList,
                ).flatten.mkString(" "),
              )(label).render

            n.onclick = { _ =>
              val newVal = (t, n).some
              if (newVal != selected.value) {
                selected.value = (t, n).some
                onChange.foreach(_(t))
              }
            }

            s.value.foreach { s =>
              if (s == t) {
                selected.value = (t, n).some
              }
            }

            n
        }

      val _label =
        JD.label(JD.id := s"$id-label", `for` := id, `class` := "kws:radio-group-label kws:field-label", label)(
          decorators.labelModifiers,
        )
      val _input =
        span(`class` := "kws:radio-group", optionNodes)(decorators.inputModifiers).render
      val _errors =
        span(JD.id := s"$id-errors", `class` := "kws:radio-group-errors kws:field-errors")(decorators.errorsModifiers)

      Widget[Maybe[T]](
        node = span(JD.id := s"$id-container", `class` := "kws:radio-group-container kws:field-container")(
          _label,
          _input,
          _errors,
        )(decorators.containerModifiers).render,
        s.value.pure[?],
      )
    }

}
object inputs extends inputs
