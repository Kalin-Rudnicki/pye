package klib.webServer

import scala.annotation.tailrec
import org.scalajs.dom._
import klib.Implicits._
import klib.fp.types._
import klib.utils.InfiniteSet
import org.scalajs.dom.raw.HTMLElement

import scala.scalajs.js

final class KeyMap(
    val onDownKeys: List[KeyMap.Key],
    val onPressKeys: List[KeyMap.Key],
    val onUpKeys: List[KeyMap.Key],
) {

  private def buildListener(lb: List[KeyMap.Key]): js.Function1[KeyboardEvent, _] = { e =>
    @tailrec
    def loop(keys: List[KeyMap.Key]): Unit =
      keys match {
        case head :: tail =>
          if (head.matches(e)) {
            if (head.preventsDefault)
              e.preventDefault()
            if (head.stopsPropagation)
              e.stopPropagation()

            head.action(e)
          } else
            loop(tail)
        case Nil =>
      }

    loop(lb.reverse)
  }

  private[webServer] def bindToWindow(): Unit = {
    window.onkeydown = buildListener(onDownKeys)
    window.onkeypress = buildListener(onPressKeys)
    window.onkeyup = buildListener(onUpKeys)
  }

  // TODO (KR) : Not sure this is working very well...
  def bindToElement(element: HTMLElement): Unit = {
    element.onkeydown = buildListener(onDownKeys)
    element.onkeypress = buildListener(onPressKeys)
    element.onkeyup = buildListener(onUpKeys)
  }

  def on(key: KeyMap.Key): KeyMap =
    key.on match {
      case KeyMap.On.KeyDown =>
        new KeyMap(
          onDownKeys = key :: onDownKeys,
          onPressKeys = onPressKeys,
          onUpKeys = onUpKeys,
        )
      case KeyMap.On.KeyPress =>
        new KeyMap(
          onDownKeys = onDownKeys,
          onPressKeys = key :: onPressKeys,
          onUpKeys = onUpKeys,
        )
      case KeyMap.On.KeyUp =>
        new KeyMap(
          onDownKeys = onDownKeys,
          onPressKeys = onPressKeys,
          onUpKeys = key :: onUpKeys,
        )
    }

  def onDown(keyCodes: KeyMap.KeyCode*)(
      name: String,
      ctrl: Maybe[Boolean] = false.some,
      shift: Maybe[Boolean] = false.some,
      preventsDefault: Boolean = true,
      stopsPropagation: Boolean = true,
  )(action: KeyboardEvent => Unit): KeyMap =
    on(
      KeyMap.Key(
        keys = InfiniteSet.Inclusive(keyCodes.toSet),
        name = name,
        action = action,
        ctrl = ctrl,
        shift = shift,
        on = KeyMap.On.KeyDown,
        preventsDefault = preventsDefault,
        stopsPropagation = stopsPropagation,
      ),
    )

  def onPress(keyCodes: KeyMap.KeyCode*)(
      name: String,
      ctrl: Maybe[Boolean] = false.some,
      shift: Maybe[Boolean] = false.some,
      preventsDefault: Boolean = true,
      stopsPropagation: Boolean = true,
  )(action: KeyboardEvent => Unit): KeyMap =
    on(
      KeyMap.Key(
        keys = InfiniteSet.Inclusive(keyCodes.toSet),
        name = name,
        action = action,
        ctrl = ctrl,
        shift = shift,
        on = KeyMap.On.KeyPress,
        preventsDefault = preventsDefault,
        stopsPropagation = stopsPropagation,
      ),
    )

  def onUp(keyCodes: KeyMap.KeyCode*)(
      name: String,
      ctrl: Maybe[Boolean] = false.some,
      shift: Maybe[Boolean] = false.some,
      preventsDefault: Boolean = true,
      stopsPropagation: Boolean = true,
  )(action: KeyboardEvent => Unit): KeyMap =
    on(
      KeyMap.Key(
        keys = InfiniteSet.Inclusive(keyCodes.toSet),
        name = name,
        action = action,
        ctrl = ctrl,
        shift = shift,
        on = KeyMap.On.KeyUp,
        preventsDefault = preventsDefault,
        stopsPropagation = stopsPropagation,
      ),
    )

}

object KeyMap {

  def empty: KeyMap =
    new KeyMap(
      onDownKeys = Nil,
      onPressKeys = Nil,
      onUpKeys = Nil,
    )

  final case class Key(
      keys: InfiniteSet[KeyCode],
      name: String,
      action: KeyboardEvent => Unit,
      ctrl: Maybe[Boolean],
      shift: Maybe[Boolean],
      preventsDefault: Boolean,
      stopsPropagation: Boolean,
      on: On,
  ) {

    def matches(e: KeyboardEvent): Boolean =
      keys.exists(_.keyCode == e.keyCode) &&
        ctrl.cata(_ == e.ctrlKey, true) &&
        shift.cata(_ == e.shiftKey, true)

  }

  sealed trait On
  object On {
    case object KeyDown extends On
    case object KeyPress extends On
    case object KeyUp extends On
  }

  final case class KeyCode(
      name: String,
      keyCode: Int,
  )
  object KeyCode {
    // format: off
    val Backspace: KeyCode = KeyCode("Backspace", 8)
    val Tab:       KeyCode = KeyCode("Tab", 9)
    val Enter:     KeyCode = KeyCode("Enter", 13)
    val Shift:     KeyCode = KeyCode("Shift", 16)
    val Ctrl:      KeyCode = KeyCode("Ctrl", 17)
    val Alt:       KeyCode = KeyCode("Alt", 18)
    val Pause:     KeyCode = KeyCode("Pause", 19)
    val CapsLock:  KeyCode = KeyCode("CapsLock", 20)
    val Escape:    KeyCode = KeyCode("Escape", 27)
    val Space:     KeyCode = KeyCode("Space", 32)
    val PageUp:    KeyCode = KeyCode("PageUp", 33)
    val PageDown:  KeyCode = KeyCode("PageDown", 34)
    val End:       KeyCode = KeyCode("End", 35)
    val Home:      KeyCode = KeyCode("Home", 36)
    val Left:      KeyCode = KeyCode("Left", 37)
    val Up:        KeyCode = KeyCode("Up", 38)
    val Right:     KeyCode = KeyCode("Right", 39)
    val Down:      KeyCode = KeyCode("Down", 40)
    val Insert:    KeyCode = KeyCode("Insert", 45)
    val Delete:    KeyCode = KeyCode("Delete", 46)
    val Num0:      KeyCode = KeyCode("Num0", 48)
    val Num1:      KeyCode = KeyCode("Num1", 49)
    val Num2:      KeyCode = KeyCode("Num2", 50)
    val Num3:      KeyCode = KeyCode("Num3", 51)
    val Num4:      KeyCode = KeyCode("Num4", 52)
    val Num5:      KeyCode = KeyCode("Num5", 53)
    val Num6:      KeyCode = KeyCode("Num6", 54)
    val Num7:      KeyCode = KeyCode("Num7", 55)
    val Num8:      KeyCode = KeyCode("Num8", 56)
    val Num9:      KeyCode = KeyCode("Num9", 57)
    val A:         KeyCode = KeyCode("A", 65)
    val B:         KeyCode = KeyCode("B", 66)
    val C:         KeyCode = KeyCode("C", 67)
    val D:         KeyCode = KeyCode("D", 68)
    val E:         KeyCode = KeyCode("E", 69)
    val F:         KeyCode = KeyCode("F", 70)
    val G:         KeyCode = KeyCode("G", 71)
    val H:         KeyCode = KeyCode("H", 72)
    val I:         KeyCode = KeyCode("I", 73)
    val J:         KeyCode = KeyCode("J", 74)
    val K:         KeyCode = KeyCode("K", 75)
    val L:         KeyCode = KeyCode("L", 76)
    val M:         KeyCode = KeyCode("M", 77)
    val N:         KeyCode = KeyCode("N", 78)
    val O:         KeyCode = KeyCode("O", 79)
    val P:         KeyCode = KeyCode("P", 80)
    val Q:         KeyCode = KeyCode("Q", 81)
    val R:         KeyCode = KeyCode("R", 82)
    val S:         KeyCode = KeyCode("S", 83)
    val T:         KeyCode = KeyCode("T", 84)
    val U:         KeyCode = KeyCode("U", 85)
    val V:         KeyCode = KeyCode("V", 86)
    val W:         KeyCode = KeyCode("W", 87)
    val X:         KeyCode = KeyCode("X", 88)
    val Y:         KeyCode = KeyCode("Y", 89)
    val Z:         KeyCode = KeyCode("Z", 90)
    val F1:        KeyCode = KeyCode("F1", 112)
    val F2:        KeyCode = KeyCode("F2", 113)
    val F3:        KeyCode = KeyCode("F3", 114)
    val F4:        KeyCode = KeyCode("F4", 115)
    val F5:        KeyCode = KeyCode("F5", 116)
    val F6:        KeyCode = KeyCode("F6", 117)
    val F7:        KeyCode = KeyCode("F7", 118)
    val F8:        KeyCode = KeyCode("F8", 119)
    val F9:        KeyCode = KeyCode("F9", 120)
    val F10:       KeyCode = KeyCode("F10", 121)
    val F11:       KeyCode = KeyCode("F11", 122)
    val F12:       KeyCode = KeyCode("F12", 123)
    // format: on
  }

}
