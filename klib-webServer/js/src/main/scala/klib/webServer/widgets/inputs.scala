package klib.webServer.widgets

import org.scalajs.dom
import org.scalajs.dom._
import scalatags.JsDom
import scalatags.JsDom.{all => JD}
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import klib.webServer._

trait inputs {

  // =====|  |=====

  final case class InputState(
      label: String,
      id: String,
      var value: String,
      labelModifiers: Seq[Modifier] = Seq.empty,
      inputModifiers: Seq[Modifier] = Seq.empty,
      errorsModifiers: Seq[Modifier] = Seq.empty,
      containerModifiers: Seq[Modifier] = Seq.empty,
  )

  private def submitEvent: Event =
    new Event(
      "submit",
      new org.scalajs.dom.raw.EventInit {
        bubbles = true
      },
    )

  // --- input ---

  val input: Widget.Builder[Maybe[String], InputState] =
    Widget[Maybe[String], InputState](_.value.ensure(_.nonEmpty).pure[?]) { s =>
      val _label =
        label(id := s"${s.id}-label", `for` := s.id, `class` := "kws:input-label kws:field-label", s.label)(s.labelModifiers)
      val _input =
        JD.input(id := s.id, `class` := "kws:input", value := s.value)(s.inputModifiers).render
      val _errors =
        span(id := s"${s.id}-errors", `class` := "kws:text-area-errors kws:field-errors")(s.errorsModifiers)

      val prevOnKeyPress = _input.onkeypress
      _input.onkeypress = { e =>
        prevOnKeyPress(e)
        if (!e.defaultPrevented) {
          if (e.key == "Enter") {
            e.preventDefault()
            s.value = _input.value
            _input.dispatchEvent(submitEvent)
          }
        }
      }

      val prevOnBlur = _input.onblur
      _input.onblur = { e =>
        prevOnBlur(e)
        if (!e.defaultPrevented) {
          s.value = _input.value
        }
      }

      span(id := s"${s.id}-container", `class` := "kws:input-container kws:field-container")(
        _label,
        _input,
        _errors,
      )(s.containerModifiers)
    }

  // --- text area ---

  val textArea: Widget.Builder[Maybe[String], InputState] =
    Widget[Maybe[String], InputState](_.value.ensure(_.nonEmpty).pure[?]) { s =>
      val _label =
        label(id := s"${s.id}-label", `for` := s.id, `class` := "kws:text-area-label kws:field-label", s.label)(
          s.labelModifiers,
        )
      val _input =
        JD.textarea(id := s.id, `class` := "kws:text-area", value := s.value)(s.inputModifiers).render
      val _errors =
        span(id := s"${s.id}-errors", `class` := "kws:text-area-errors kws:field-errors")(s.errorsModifiers)

      val prevOnKeyPress = _input.onkeypress
      _input.onkeypress = { e =>
        prevOnKeyPress(e)
        if (!e.defaultPrevented) {
          if (e.key == "Enter") {
            e.preventDefault()
            s.value = _input.value
            _input.dispatchEvent(submitEvent)
          }
        }
      }

      val prevOnBlur = _input.onblur
      _input.onblur = { e =>
        prevOnBlur(e)
        if (!e.defaultPrevented) {
          s.value = _input.value
        }
      }

      span(id := s"${s.id}-container", `class` := "kws:text-area-container kws:field-container")(
        _label,
        _input,
        _errors,
      )(s.containerModifiers)
    }

  // =====|  |=====

  final case class RadioGroupState[T](
      label: String,
      id: String,
      options: Array[(String, T)],
      var value: Maybe[T],
      onChange: Maybe[T => Unit] = None,
      labelModifiers: Seq[Modifier] = Seq.empty,
      inputModifiers: Seq[Modifier] = Seq.empty,
      errorsModifiers: Seq[Modifier] = Seq.empty,
      containerModifiers: Seq[Modifier] = Seq.empty,
  )

  def radioGroup[T]: Widget.Builder[Maybe[T], RadioGroupState[T]] =
    Widget[Maybe[T], RadioGroupState[T]](_.value.pure[?]) { s =>
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
        s.options.zipWithIndex.map {
          case ((label, t), i) =>
            val n =
              span(
                `class` := List(
                  List(
                    "clickable",
                    "kws:radio-group__button",
                  ),
                  (i == 0).maybe("kws:radio-group__button--first").toList,
                  (i == s.options.length - 1).maybe("kws:radio-group__button--last").toList,
                ).flatten.mkString(" "),
              )(label).render

            n.onclick = { _ =>
              val newVal = (t, n).some
              if (newVal != selected.value) {
                selected.value = (t, n).some
                s.onChange.foreach(_(t))
              }
            }

            s.value.foreach { s =>
              if (s == t) {
                println(s"s: $s, t: $t, s == t: ${s == t}")
                selected.value = (t, n).some
              }
            }

            n
        }

      val _label =
        label(id := s"${s.id}-label", `for` := s.id, `class` := "kws:radio-group-label kws:field-label", s.label)(
          s.labelModifiers,
        )
      val _input =
        span(`class` := "kws:radio-group", optionNodes)(s.inputModifiers).render
      val _errors =
        span(id := s"${s.id}-errors", `class` := "kws:radio-group-errors kws:field-errors")(s.errorsModifiers)

      span(id := s"${s.id}-container", `class` := "kws:radio-group-container kws:field-container")(
        _label,
        _input,
        _errors,
      )(s.containerModifiers)
    }

}
object inputs extends inputs
