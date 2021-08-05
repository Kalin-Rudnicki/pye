package pye.legacyWidgets

import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html.Input
import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag
import scalatags.JsDom.{all => JD}

import klib.Implicits._
import klib.fp.types._

trait inputs {

  final case class Decorators(
      labelModifiers: Seq[Modifier] = Seq.empty,
      inputModifiers: Seq[Modifier] = Seq.empty,
      errorsModifiers: Seq[Modifier] = Seq.empty,
      containerModifiers: Seq[Modifier] = Seq.empty,
  )

  // =====|  |=====

  // --- input ---

  // TODO (KR) : This could use some cleaning up...
  @deprecated(message = "Use new Widget", since = "3.0.0")
  def file(
      label: String,
      id: String,
      decorators: Decorators,
  ): Widget.Builder[Maybe[dom.File], Var[dom.FileList]] = {
    def convertFileList(fl: dom.FileList): ?[Maybe[dom.File]] =
      (fl != null && fl.length == 1).maybe(fl(0)).pure[?]

    Widget
      .Builder[Maybe[dom.File], Var[dom.FileList]] { s =>
        Widget(convertFileList(s.value)) {
          val _label =
            JD.label(JD.id := s"$id-label", `for` := id, `class` := s"kws:file-input-label kws:field-label", label)(
              decorators.labelModifiers,
            )
          val _input =
            JD.input(JD.id := id, `class` := s"kws:file-input", `type` := "file")(decorators.inputModifiers).render

          val _errors =
            span(JD.id := s"$id-errors", `class` := s"kws:file-input-errors kws:field-errors")(decorators.errorsModifiers)

          val prevOnChange = _input.onchange
          _input.onchange = { e =>
            if (prevOnChange != null)
              prevOnChange(e)
            if (!e.defaultPrevented) {
              s.silentValue = _input.files
            }
          }

          _input.files = s.value

          span(JD.id := s"$id-container", `class` := s"kws:file-input-container kws:field-container")(
            _label,
            _input,
            _errors,
          )(decorators.containerModifiers).render
        }
      }
      .labelErrors(label)
  }

  @deprecated(message = "Use new Widget", since = "3.0.0")
  private def textWidget(
      _type: String,
      __input: TypedTag[Input],
      filterSubmit: KeyboardEvent => Boolean,
  )(
      label: String,
      id: String,
      decorators: Decorators,
  ): Widget.Builder[Maybe[String], Var[String]] =
    Widget
      .Builder[Maybe[String], Var[String]] { s =>
        Widget(s.value.ensure(_.nonEmpty).pure[?]) {
          val _label =
            JD.label(JD.id := s"$id-label", `for` := id, `class` := s"kws:${_type}-label kws:field-label", label)(
              decorators.labelModifiers,
            )
          val _input = __input(JD.id := id, `class` := s"kws:${_type}")(decorators.inputModifiers).render

          val _errors =
            span(JD.id := s"$id-errors", `class` := s"kws:${_type}-errors kws:field-errors")(decorators.errorsModifiers)

          _input.onkeydown = _.stopPropagation()

          val prevOnKeyPress = _input.onkeypress
          _input.onkeypress = { e =>
            if (prevOnKeyPress != null)
              prevOnKeyPress(e)
            if (!e.defaultPrevented) {
              if (filterSubmit(e)) {
                e.preventDefault()
                s.silentValue = _input.value
                _input.dispatchEvent(events.submitEvent)
              }
            }
            e.stopPropagation()
          }

          val prevOnBlur = _input.onblur
          _input.onblur = { e =>
            if (prevOnBlur != null)
              prevOnBlur(e)
            if (!e.defaultPrevented) {
              s.silentValue = _input.value
            }
          }

          _input.value = s.value

          span(JD.id := s"$id-container", `class` := s"kws:${_type}-container kws:field-container")(
            _label,
            _input,
            _errors,
          )(decorators.containerModifiers).render
        }
      }
      .labelErrors(label)

  @deprecated(message = "Use new Widget", since = "3.0.0")
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

  @deprecated(message = "Use new Widget", since = "3.0.0")
  def textArea(
      label: String,
      id: String,
      decorators: Decorators = Decorators(),
  ): Widget.Builder[Maybe[String], Var[String]] =
    textWidget(
      "text-area",
      JD.textarea.asInstanceOf[TypedTag[Input]],
      e => e.key == "Enter" && e.ctrlKey,
    )(
      label,
      id,
      decorators,
    )

  // =====|  |=====

  @deprecated(message = "Use new Widget", since = "3.0.0")
  def radioGroup[T](
      label: String,
      id: String,
      options: Array[(String, T)],
      onChange: Maybe[Maybe[T] => Unit] = None,
      allowUnset: Boolean = false,
      decorators: Decorators = Decorators(),
  ): Widget.Builder[Maybe[T], Var[Maybe[T]]] =
    Widget
      .Builder[Maybe[T], Var[Maybe[T]]] { s =>
        Widget[Maybe[T]](
          s.value.pure[?],
        ) {
          // TODO (KR) : ...
          object selected {
            private var _value: Maybe[(T, dom.html.Span)] = None

            def value: Maybe[(T, dom.html.Span)] = _value
            def value_=(v: Maybe[(T, dom.html.Span)]): Unit = {
              s.silentValue = v.map(_._1)

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

                n.onclick = { e =>
                  val newVal = (t, n).some
                  if (newVal != selected.value) {
                    selected.value = (t, n).some
                    onChange.foreach(_(t.some))
                  } else if (allowUnset && e.ctrlKey) {
                    selected.value = None
                    onChange.foreach(_(None))
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

          span(JD.id := s"$id-container", `class` := "kws:radio-group-container kws:field-container")(
            _label,
            _input,
            _errors,
          )(decorators.containerModifiers).render
        }
      }
      .labelErrors(label)

  @deprecated(message = "Use new Widget", since = "3.0.0")
  def toggleButton(
      buttonLabel: String,
      onToggle: Maybe[Boolean => Unit] = None,
      buttonDecorators: Seq[Modifier] = Seq.empty,
  ): Widget.Builder[Boolean, Var[Boolean]] =
    Widget.Builder[Boolean, Var[Boolean]] { s =>
      Widget[Boolean](s.value.pure[?]) {
        val btn =
          button(
            `class` := List(
              "kws:toggle-button".some,
              s.value.maybe("kws:toggle-button--true"),
              (!s.value).maybe("kws:toggle-button--false"),
            ).flatMap(_.toOption)
              .mkString(" "),
          )(
            buttonLabel,
          )(buttonDecorators).render

        btn.onclick = e => {
          e.stopPropagation()
          s.value = !s.value
          btn.classList.toggle("kws:toggle-button--true")
          btn.classList.toggle("kws:toggle-button--false")

          onToggle.foreach(_(s.value))
        }

        btn
      }
    }

}
object inputs extends inputs