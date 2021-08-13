package pye.widgets

import scala.scalajs.js

import org.scalajs.dom
import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._
import pye._
import pye.Implicits._
import pye.widgets.modifiers._

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
      decorator: Modifier = Seq.empty[Modifier],
  ): Widget.Submit[Maybe[V], String] =
    Widget.builder
      .withState[String]
      .submitAction
      .rsElement { rh => s =>
        var savedTimeout: Maybe[Int] = None
        val _input = inputTag(decorator).render

        // TODO (KR) : Make sure state is updated before re-rendering
        def updateStateRaise(): Raise.UpdateState[String] =
          Raise.UpdateState[String](_ => _input.value, reRender = false)

        def updateState(): Unit =
          rh._handleRaise(updateStateRaise())

        _input.value = s
        _input.onkeypress = { e =>
          if (filterSubmit(e)) {
            e.preventDefault()
            rh.raise(
              updateStateRaise(),
              Raise.Action(CommonRaise.Submit),
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
      .withValue(_.ensure(_.nonEmpty).map(implicitly[DecodeString[V]].decode).traverse)

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
      inputTag = textarea.asInstanceOf,
      filterSubmit = e => e.keyCode == KeyMap.KeyCode.Enter.keyCode && e.ctrlKey,
    )(
      updateOn = updateOn,
      decorator = Seq[Modifier](
        PyeS.`pye:text-area`,
        decorator,
      ),
    )

  // ---  ---

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
  ): Widget[Maybe[VS[File]], Maybe[VS[File]], Nothing] = {
    def ensureFileType(
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

    Widget.builder
      .withState[Maybe[VS[File]]]
      .withAction[Nothing]
      .rsElement { rh => s =>
        def setState(fileList: List[File]): Unit = {
          val res =
            for {
              fromList <- fromFileList(fileList)
              ensured <- fileType match {
                case Some(fileType) =>
                  fromList.map(_.map(ensureFileType(fileType, _)).traverse).traverse
                case None =>
                  fromList.pure[?]
              }
            } yield ensured

          res match {
            case Alive(res) =>
              rh.setState(res)
            case Dead(errors) =>
              rh.raises(errors.map(Raise.DisplayMessage.fromThrowable))
          }
        }

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

          setState(fileListToList(files))
        }

        val fileInput =
          div(
            hiddenFileInput,
            PyeS.`pye:file-input`,
            oncontextmenu := { (e: Event) =>
              e.preventDefault()
              setState(Nil)
            },
          )(
            s.cata(
              vsToList(_).map { file =>
                span(PyeS.`pye:file-input`.file)(file.name)
              },
              onEmpty.toOption,
            ),
          ).render

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
          setState(
            e.ctrlKey.maybe(s).flatten.cata(vsToList, Nil) ++
              fileListToList(e.dataTransfer.files),
          )
        }

        fileInput
      }
      .withValue { s =>
        s.pure[?]
      }
  }

  private def defaultOnEmpty: Element =
    span("Click or drag to select file").render

  def fileInputW(
      fileType: Maybe[String] = None,
      onEmpty: Maybe[Element] = defaultOnEmpty.some,
  ): Widget[Maybe[File], Maybe[File], Nothing] =
    genFileInputW[Identity](
      fromFileList = {
        case Nil =>
          None.pure[?]
        case head :: Nil =>
          head.some.pure[?]
        case _ =>
          ?.dead(Message("This input does not support multiple files"))
      },
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

  // ---  ---

  def toggleButtonW(
      buttonLabel: String,
      buttonDecorator: Modifier = Seq.empty[Modifier],
  ): Widget[Boolean, Boolean, Nothing] =
    Widget.builder
      .withState[Boolean]
      .noAction
      .rsElement { rh => s =>
        button(
          PyeS.`pye:toggle-button`.--?(
            s -> PyeS.`pye:toggle-button`.`true`,
            !s -> PyeS.`pye:toggle-button`.`false`,
          ),
          onclick := { (_: Event) =>
            rh.setState(!s)
          },
        )(buttonLabel)(buttonDecorator).render
      }
      .withValue(_.pure[?])

  // ---  ---

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
                      rh.raise(Raise.UpdateState[Maybe[T]](_ => None))
                  } else
                    rh.raise(Raise.UpdateState[Maybe[T]](_ => t.some))
                },
              )(_label)(elementDecorator)
          }

        span(optionNodes)(actualDecorators).render
      }
      .withValue(_.pure[?])
  }

}
object inputs extends inputs
