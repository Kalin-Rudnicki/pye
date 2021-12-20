package pye.widgets

import java.time._
import java.time.format.DateTimeFormatter

import scala.annotation.tailrec
import scala.scalajs.js

import org.scalajs.dom
import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._
import pye._
import pye.Implicits._
import pye.KeyMap.KeyCode
import pye.widgets.modifiers._

trait inputs {

  // =====| Text |=====

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
      decorator: Modifier = Seq.empty[Modifier],
  ): Widget.Submit[Maybe[V], String] =
    Widget.builder
      .withState[String]
      .submitAction
      .rsElement { rh => s =>
        var savedTimeout: Maybe[Int] = None
        val _input = inputTag(decorator).render

        var lastRaised = s

        def updateStateRaise(): Maybe[Raise.UpdateState[String]] =
          (_input.value != lastRaised).maybe {
            lastRaised = _input.value
            Raise.setState[String](_input.value).propagate
          }

        def updateState(): Unit =
          updateStateRaise().foreach(rh.raise(_))

        _input.value = s
        _input.onkeyup = { e =>
          if (filterSubmit(e)) {
            e.preventDefault()
            rh.raises(
              List(
                updateStateRaise(),
                Raise.Action(CommonRaise.Submit).some,
              ).flatMap(_.toOption),
            )
          } else
            updateOn match {
              case UpdateOn.KeyPress(timeout) => // TODO (KR) : Doesnt seem to be picking up on backspaces...
                timeout match {
                  case Some(timeout) =>
                    savedTimeout.foreach(window.clearTimeout)
                    savedTimeout = window
                      .setTimeout(
                        { () =>
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
      .withValue {
        _.ensure(_.nonEmpty)
          .map(implicitly[DecodeString[V]].decode)
          .traverse
      }

  def inputW[V: DecodeString](
      updateOn: UpdateOn = UpdateOn.Blur,
      decorator: Modifier = Seq.empty[Modifier],
  ): Widget.Submit[Maybe[V], String] =
    genInputW[V](
      inputTag = input,
      filterSubmit = e => e.keyCode == KeyMap.KeyCode.Enter.keyCode,
    )(
      updateOn = updateOn,
      decorator = Seq[Modifier](
        PyeS.`pye:input`,
        decorator,
      ),
    )

  def textAreaW[V: DecodeString](
      updateOn: UpdateOn = UpdateOn.Blur,
      decorator: Modifier = Seq.empty[Modifier],
  ): Widget.Submit[Maybe[V], String] =
    genInputW[V](
      inputTag = textarea.asInstanceOf[ConcreteHtmlTag[dom.html.Input]],
      filterSubmit = e => e.keyCode == KeyMap.KeyCode.Enter.keyCode && e.ctrlKey,
    )(
      updateOn = updateOn,
      decorator = Seq[Modifier](
        PyeS.`pye:text-area`,
        decorator,
      ),
    )

  // =====| Editable Text |=====

  final case class EditableText(
      current: String,
      edit: Maybe[String],
  )

  private def genEditableInputW[V: DecodeString](
      inputTag: ConcreteHtmlTag[dom.html.Input],
      filterSubmit: KeyboardEvent => Boolean,
  )(
      updateOn: UpdateOn,
      inputDecorator: Modifier = Seq.empty[Modifier],
      currentDecorator: Modifier = Seq.empty[Modifier],
  ): Widget.Submit[Maybe[V], EditableText] =
    Widget.builder
      .withState[EditableText]
      .submitAction
      .rsElement { rh => s =>
        s.edit match {
          case Some(edit) =>
            var savedTimeout: Maybe[Int] = None
            val _input = inputTag(inputDecorator).render

            var lastRaised = edit

            def updateStateRaise(): Maybe[Raise.UpdateState[EditableText]] =
              (_input.value != lastRaised).maybe {
                lastRaised = _input.value
                Raise.updateState[EditableText](_.copy(edit = _input.value.some)).propagate
              }

            def updateState(): Unit =
              updateStateRaise().foreach(rh.raise(_))

            _input.value = edit
            _input.onkeyup = { e =>
              if (filterSubmit(e)) {
                e.preventDefault()
                rh.raises(
                  List(
                    updateStateRaise(),
                    Raise.Action(CommonRaise.Submit).some,
                  ).flatMap(_.toOption),
                )
              } else if (e.keyCode == KeyCode.Escape.keyCode)
                rh.raise(
                  Raise.Raw { AsyncIO { savedTimeout.foreach(window.clearTimeout) } },
                  Raise.updateState[EditableText](_.copy(edit = None)),
                )
              else
                updateOn match {
                  case UpdateOn.KeyPress(timeout) => // TODO (KR) : Doesnt seem to be picking up on backspaces...
                    timeout match {
                      case Some(timeout) =>
                        savedTimeout.foreach(window.clearTimeout)
                        savedTimeout = window
                          .setTimeout(
                            { () =>
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
          case None =>
            span(
              onclick := { (_: Event) =>
                rh.state.update(s => s.copy(edit = s.current.some))
              },
              currentDecorator,
            )(s.current).render
        }
      }
      .withValue { s =>
        def toV(str: String): ?[Maybe[V]] =
          str
            .ensure(_.nonEmpty)
            .map(implicitly[DecodeString[V]].decode)
            .traverse

        s.edit.cata(toV, toV(s.current))
      }

  def editableInputW[V: DecodeString](
      updateOn: UpdateOn = UpdateOn.Blur,
      inputDecorator: Modifier = Seq.empty[Modifier],
      currentDecorator: Modifier = Seq.empty[Modifier],
  ): Widget.Submit[Maybe[V], EditableText] =
    genEditableInputW[V](
      inputTag = input,
      filterSubmit = e => e.keyCode == KeyMap.KeyCode.Enter.keyCode,
    )(
      updateOn = updateOn,
      inputDecorator = Seq[Modifier](
        PyeS.`pye:input`,
        inputDecorator,
      ),
      currentDecorator = currentDecorator,
    )

  def editableTextAreaW[V: DecodeString](
      updateOn: UpdateOn = UpdateOn.Blur,
      inputDecorator: Modifier = Seq.empty[Modifier],
      currentDecorator: Modifier = Seq.empty[Modifier],
  ): Widget.Submit[Maybe[V], EditableText] =
    genEditableInputW[V](
      inputTag = textarea.asInstanceOf[ConcreteHtmlTag[dom.html.Input]],
      filterSubmit = e => e.keyCode == KeyMap.KeyCode.Enter.keyCode && e.ctrlKey,
    )(
      updateOn = updateOn,
      inputDecorator = Seq[Modifier](
        PyeS.`pye:text-area`,
        inputDecorator,
      ),
      currentDecorator = currentDecorator,
    )

  // =====| File |=====

  private def ensureFileType(
      fileType: String,
      file: File,
  ): ?[File] =
    if (fileType.startsWith("."))
      file
        .ensure(_.name.endsWith(fileType))
        .toEA(Message(s"Invalid file-type: ${file.name}"))
    else {
      // TODO (KR) :
      file.pure[?]
    }

  private def convertFileList[VS[_]](
      fileList: List[File],
      fromFileList: List[File] => ?[Maybe[VS[File]]],
      fileType: Maybe[String],
  )(implicit
      vsFunctor: Functor[VS],
      vsTraverseList: Traverse[VS, ?],
  ): ?[Maybe[VS[File]]] =
    for {
      fromList <- fromFileList(fileList)
      ensured <- fileType match {
        case Some(fileType) =>
          fromList.map(_.map(ensureFileType(fileType, _)).traverse).traverse
        case None =>
          fromList.pure[?]
      }
    } yield ensured

  private def makeFileInput[S](
      fileType: Maybe[String],
      allowMulti: Boolean,
  )(
      onSelectedFromWindowF: List[File] => Unit,
      onDropF: (Boolean, S, List[File]) => Unit,
      buttonModF: S => Modifier,
  )(s: S): Widget.ElemT = {
    def fileListToList(files: FileList): List[File] =
      0.until(files.length)
        .toList
        .map(files(_))

    val hiddenFileInput =
      input(
        `type` := "file",
        fileType.map(accept := _).toOption,
        allowMulti.maybe(multiple).toOption,
        display := "none",
      ).render

    hiddenFileInput.onchange = { e =>
      val files: FileList =
        e.target
          .asInstanceOf[js.Dynamic]
          .files
          .asInstanceOf[FileList]

      onSelectedFromWindowF(fileListToList(files))
    }

    val fileInput =
      div(
        hiddenFileInput,
        PyeS.`pye:file-input`,
      )(buttonModF(s)).render

    fileInput.onclick = { _ =>
      // TODO (KR) : Ctrl + click to add
      hiddenFileInput.click()
    }
    fileInput.ondragover = { e =>
      e.preventDefault()
      fileInput.classList.add(PyeS.`pye:file-input`.!.m(_.`dragged-into`).classes)
    }
    fileInput.ondragleave = { _ =>
      fileInput.classList.remove(PyeS.`pye:file-input`.!.m(_.`dragged-into`).classes)
    }
    fileInput.ondragend = { _ =>
      fileInput.classList.remove(PyeS.`pye:file-input`.!.m(_.`dragged-into`).classes)
    }
    fileInput.ondrop = { e =>
      e.preventDefault()
      onDropF(e.ctrlKey, s, fileListToList(e.dataTransfer.files))
    }

    fileInput
  }

  private def fileListToFile(fileList: List[File]): ?[Maybe[File]] =
    fileList match {
      case Nil =>
        None.pure[?]
      case head :: Nil =>
        head.some.pure[?]
      case _ =>
        ?.dead(Message("This input does not support multiple files"))
    }

  // --- Drop Submits ---

  private def genFileDropW[VS[_]](
      fromFileList: List[File] => ?[Maybe[VS[File]]],
      allowMulti: Boolean,
  )(
      fileType: Maybe[String],
      decorator: Modifier,
  )(implicit
      vsFunctor: Functor[VS],
      vsTraverseList: Traverse[VS, ?],
  ): Widget.StatelessWidget[VS[File]] =
    new Widget.StatelessWidget[VS[File]] {
      override final def withState[S]: Widget[Unit, S, VS[File]] =
        Widget.builder
          .withState[S]
          .withAction[VS[File]]
          .rsElement { rh => s =>
            def raiseFiles(fileList: List[File]): Unit =
              convertFileList[VS](fileList, fromFileList, fileType)
                .flatMap(_.toEA(Message("Can not drop 0 files"))) match {
                case Alive(res) =>
                  rh.raiseAction(res)
                case Dead(errors) =>
                  rh.raises(errors.map(Raise.DisplayMessage.fromThrowable))
              }

            makeFileInput[S](
              fileType,
              allowMulti,
            )(
              raiseFiles,
              (_, _, files) => raiseFiles(files),
              _ => decorator,
            )(s)
          }
          .noValue
    }

  def fileDropW(
      fileType: Maybe[String] = None,
      decorator: Modifier = defaultOnEmpty,
  ): Widget.StatelessWidget[File] =
    genFileDropW[Identity](
      fromFileList = fileListToFile,
      allowMulti = false,
    )(
      fileType = fileType,
      decorator = decorator,
    )

  def multiFileDropW(
      fileType: Maybe[String] = None,
      decorator: Modifier = defaultOnEmpty,
  ): Widget.StatelessWidget[NonEmptyList[File]] =
    genFileDropW[NonEmptyList](
      fromFileList = _.toNel.pure[?],
      allowMulti = true,
    )(
      fileType = fileType,
      decorator = decorator,
    )

  // --- Drop Updates State ---

  private def genFileInputW[VS[_]](
      fromFileList: List[File] => ?[Maybe[VS[File]]],
      vsToList: VS[File] => List[File], // TODO (KR) : Ability to vary appearance
      allowMulti: Boolean,
  )(
      fileType: Maybe[String],
      onEmpty: Maybe[Element],
  )(implicit
      vsFunctor: Functor[VS],
      vsTraverseList: Traverse[VS, ?],
  ): Widget[Maybe[VS[File]], Maybe[VS[File]], Nothing] =
    Widget.builder
      .withState[Maybe[VS[File]]]
      .withAction[Nothing]
      .rsElement { rh => s =>
        def setState(fileList: List[File]): Unit =
          convertFileList[VS](fileList, fromFileList, fileType) match {
            case Alive(res) =>
              rh.state.set(res)
            case Dead(errors) =>
              rh.raises(errors.map(Raise.DisplayMessage.fromThrowable))
          }

        makeFileInput[Maybe[VS[File]]](
          fileType,
          allowMulti,
        )(
          setState,
          (ctrlKey, s, files) => setState(ctrlKey.maybe(s).flatten.cata(vsToList, Nil) ++ files),
          _.cata(
            vsToList(_).map { file =>
              span(PyeS.`pye:file-input`.file)(file.name)
            },
            onEmpty.toOption,
          ),
        )(s)
      }
      .withValue { _.pure[?] }

  private def defaultOnEmpty: Element =
    span("Click or drag to select file").render

  def fileInputW(
      fileType: Maybe[String] = None,
      onEmpty: Maybe[Element] = defaultOnEmpty.some,
  ): Widget[Maybe[File], Maybe[File], Nothing] =
    genFileInputW[Identity](
      fromFileList = fileListToFile,
      vsToList = _ :: Nil,
      allowMulti = false,
    )(
      fileType = fileType,
      onEmpty = onEmpty,
    )

  def multiFileInputW(
      fileType: Maybe[String] = None,
      onEmpty: Maybe[Element] = defaultOnEmpty.some,
  ): Widget[Maybe[NonEmptyList[File]], Maybe[NonEmptyList[File]], Nothing] =
    genFileInputW[NonEmptyList](
      fromFileList = _.toNel.pure[?],
      vsToList = _.toList,
      allowMulti = true,
    )(
      fileType = fileType,
      onEmpty = onEmpty,
    )

  // =====| Buttons |=====

  def toggleButtonElement(
      buttonLabel: String,
      buttonDecorator: Modifier = Seq.empty[Modifier],
  )(
      s: Boolean,
      onClick: () => Unit,
  ): dom.html.Button =
    button(
      PyeS.`pye:toggle-button`.--?(
        s -> PyeS.`pye:toggle-button`.`true`,
        !s -> PyeS.`pye:toggle-button`.`false`,
      ),
      onclick := { (_: Event) =>
        onClick()
      },
    )(buttonLabel)(buttonDecorator).render

  def toggleButtonW(
      buttonLabel: String,
      buttonDecorator: Modifier = Seq.empty[Modifier],
  ): Widget[Boolean, Boolean, Nothing] =
    Widget.builder
      .withState[Boolean]
      .noAction
      .rsElement { rh => s =>
        toggleButtonElement(
          buttonLabel,
          buttonDecorator,
        )(
          s,
          () => rh.state.update(!_),
        )
      }
      .withValue(_.pure[?])

  def radioGroupW[T](
      options: Array[(String, T)],
      allowUnset: Boolean = false,
      decorator: Modifier = Seq.empty[Modifier],
      elementDecorator: Modifier = Seq.empty[Modifier],
  ): Widget[Maybe[T], Maybe[T], Nothing] = {
    val actualDecorators = Seq[Modifier](PyeS.`pye:radio-group`, decorator)

    Widget.builder
      .withState[Maybe[T]]
      .withAction[Nothing]
      .rsElement { rh => s =>
        val selectedIndex: Int = s.cata(s => options.indexWhere(_._2 == s), -1)

        val optionNodes =
          options.zipWithIndex.map {
            case ((_label, t), i) =>
              span(
                PyeS.`pye:radio-group`
                  .e(_.option)
                  .--?(
                    (i == selectedIndex) -> PyeS.`pye:radio-group`.option.selected,
                    (i == 0) -> PyeS.`pye:radio-group`.option.first,
                    (i == options.length - 1) -> PyeS.`pye:radio-group`.option.last,
                  ),
                onclick := { (e: MouseEvent) =>
                  if (e.ctrlKey) {
                    if (allowUnset)
                      rh.raise(Raise.setState[Maybe[T]](None))
                  } else
                    rh.raise(Raise.setState[Maybe[T]](t.some))
                },
              )(_label)(elementDecorator)
          }

        span(optionNodes)(actualDecorators).render
      }
      .withValue(_.pure[?])
  }

  // =====| Date Picker |=====

  final case class DatePicker(
      selected: Maybe[LocalDate],
      yearMonth: YearMonth,
      editing: Boolean,
  )
  object DatePicker {

    private def now(f: LocalDate => Maybe[LocalDate]): DatePicker = {
      val currentDate = LocalDate.now
      DatePicker(
        selected = f(currentDate),
        yearMonth = YearMonth.of(currentDate.getYear, currentDate.getMonthValue),
        editing = false,
      )
    }

    def nowNone: DatePicker =
      now(_ => None)

    def nowSome: DatePicker =
      now(_.some)

  }

  def datePickerW(
      dateFormatter: DateTimeFormatter,
  ): Widget[Maybe[LocalDate], DatePicker, Maybe[LocalDate]] =
    Widget.builder
      .withState[DatePicker]
      .withAction[Maybe[LocalDate]]
      .rsElement { rh => s =>
        val selected: Frag =
          span(
            PyeS.`pye:date-picker`.selected,
            onclick := { (e: MouseEvent) =>
              if (e.ctrlKey) rh.state.update(_.copy(yearMonth = YearMonth.now, editing = true))
              else rh.state.update(s => s.copy(editing = !s.editing))
            },
            oncontextmenu := { (e: Event) =>
              e.preventDefault()
              rh.raise(
                Raise.updateState[DatePicker](_.copy(selected = None)),
                Raise.Action(None),
              )
            },
          )(
            s.selected.map(dateFormatter.format).toOption,
          ).render
        def editing: Frag = {
          val firstDayOfMonth = s.yearMonth.atDay(1)
          val firstDayOffset = firstDayOfMonth.getDayOfWeek.getValue % 7

          def arrowButton(label: String)(ymF: YearMonth => YearMonth): Frag =
            span(
              PyeS.`pye:date-picker`.`arrow-button`,
              onclick := { (_: Event) =>
                rh.state.update(s => s.copy(yearMonth = ymF(s.yearMonth)))
              },
            )(label)

          def dayOfWeekLabel(label: String): Frag =
            span(
              PyeS.`pye:date-picker`.`day-of-week`,
            )(label)

          def dayButton(
              date: LocalDate,
          ): Frag = {
            span(
              PyeS.`pye:date-picker`
                .e(_.`day-button`)
                .--?(
                  s.selected.cata(_ == date, false) ->
                    PyeS.`pye:date-picker`.`day-button`.selected,
                  (YearMonth.of(date.getYear, date.getMonthValue) == s.yearMonth) ->
                    PyeS.`pye:date-picker`.`day-button`.`in-month`,
                ),
              onclick := { (_: Event) =>
                rh.raise(
                  Raise.setState[DatePicker](
                    DatePicker(
                      date.some,
                      YearMonth.of(date.getYear, date.getMonthValue),
                      false,
                    ),
                  ),
                  Raise.Action(date.some),
                )
              },
            )(date.getDayOfMonth)
          }

          @tailrec
          def makeButtons(
              start: Int,
              stack: List[Frag],
          ): Frag =
            if (start <= s.yearMonth.lengthOfMonth)
              makeButtons(
                start + 7,
                div(
                  0.until(7).map { dayInWeek =>
                    dayButton(firstDayOfMonth.plusDays(start + dayInWeek))
                  },
                ) :: stack,
              )
            else
              stack.reverse

          div(
            PyeS.`pye:date-picker`.editing,
          )(
            div(
              PyeS.`pye:date-picker`.`editing-header`,
            )(
              arrowButton("<<")(_.minusYears(1)),
              arrowButton("<")(_.minusMonths(1)),
              span(
                PyeS.`pye:date-picker`.`year-month`,
              )(
                s"${s.yearMonth.getMonth.name().toLowerCase.capitalize} ${s.yearMonth.getYear}",
              ),
              arrowButton(">")(_.plusMonths(1)),
              arrowButton(">>")(_.plusYears(1)),
            ),
            div(
              div(
                dayOfWeekLabel("Sun"),
                dayOfWeekLabel("Mon"),
                dayOfWeekLabel("Tues"),
                dayOfWeekLabel("Wed"),
                dayOfWeekLabel("Thurs"),
                dayOfWeekLabel("Fri"),
                dayOfWeekLabel("Sat"),
              ),
              div(
                PyeS.`pye:date-picker`.`editing-under-header`,
              )(
                makeButtons(-firstDayOffset, Nil),
              ),
            ),
          )
        }

        div(
          PyeS.`pye:date-picker`,
        )(
          selected,
          s.editing.maybe(editing).toOption,
        ).render
      }
      .withValue(_.selected.pure[?])

}
object inputs extends inputs
