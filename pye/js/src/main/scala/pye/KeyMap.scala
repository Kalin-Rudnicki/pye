package pye

import scala.annotation.tailrec

import org.scalajs.dom._

import klib.Implicits._
import klib.fp.types._
import klib.utils.InfiniteSet

final class KeyMap[S, +A](
    val onDownKeys: List[KeyMap.Key[S, A]],
    val onPressKeys: List[KeyMap.Key[S, A]],
    val onUpKeys: List[KeyMap.Key[S, A]],
) {

  private def buildListener[A2 >: A](rh: RaiseHandler[S, A2], lb: List[KeyMap.Key[S, A2]]): KeyboardEvent => Unit = { e =>
    @tailrec
    def loop(keys: List[KeyMap.Key[S, A2]]): Unit =
      keys match {
        case head :: tail =>
          if (head.matches(e)) {
            if (head.preventsDefault)
              e.preventDefault()
            if (head.stopsPropagation)
              e.stopPropagation()

            rh.raises(head.action(e))
          } else
            loop(tail)
        case Nil =>
      }

    loop(lb.reverse)
  }

  def withRaiseHandler[A2 >: A](rh: RaiseHandler[S, A2]): BindableKeyMap =
    new BindableKeyMap {
      override private[pye] val onKeyDown: KeyboardEvent => Unit = buildListener(rh, onDownKeys)
      override private[pye] val onKeyPress: KeyboardEvent => Unit = buildListener(rh, onPressKeys)
      override private[pye] val onKeyUp: KeyboardEvent => Unit = buildListener(rh, onUpKeys)
    }

  def on[A2 >: A](key: KeyMap.Key[S, A2]): KeyMap[S, A2] =
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

  def onDown[A2 >: A](
      action: KeyboardEvent => List[Raise[S, A2]],
  )(
      name: String,
      ctrl: Maybe[Boolean] = false.some,
      shift: Maybe[Boolean] = false.some,
      preventsDefault: Boolean = true,
      stopsPropagation: Boolean = true,
  )(
      keyCodes: KeyMap.KeyCode*,
  ): KeyMap[S, A2] =
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

  def onPress[A2 >: A](
      action: KeyboardEvent => List[Raise[S, A2]],
  )(
      name: String,
      ctrl: Maybe[Boolean] = false.some,
      shift: Maybe[Boolean] = false.some,
      preventsDefault: Boolean = true,
      stopsPropagation: Boolean = true,
  )(
      keyCodes: KeyMap.KeyCode*,
  ): KeyMap[S, A2] =
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

  def onUp[A2 >: A](
      action: KeyboardEvent => List[Raise[S, A2]],
  )(
      name: String,
      ctrl: Maybe[Boolean] = false.some,
      shift: Maybe[Boolean] = false.some,
      preventsDefault: Boolean = true,
      stopsPropagation: Boolean = true,
  )(
      keyCodes: KeyMap.KeyCode*,
  ): KeyMap[S, A2] =
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

  def empty[S]: KeyMap[S, Nothing] =
    new KeyMap(
      onDownKeys = Nil,
      onPressKeys = Nil,
      onUpKeys = Nil,
    )

  final case class Key[S, +A](
      keys: InfiniteSet[KeyCode],
      name: String,
      action: KeyboardEvent => List[Raise[S, A]],
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

  // =====|  |=====

}

sealed trait BindableKeyMap {
  private[pye] val onKeyDown: KeyboardEvent => Unit
  private[pye] val onKeyPress: KeyboardEvent => Unit
  private[pye] val onKeyUp: KeyboardEvent => Unit

  def bindTo[N](n: N)(implicit bindTo: BindableKeyMap.BindTo[N]): Unit = {
    bindTo.setKeyDown(n, onKeyDown)
    bindTo.setKeyPress(n, onKeyPress)
    bindTo.setKeyUp(n, onKeyUp)
  }

}
object BindableKeyMap {

  trait BindTo[N] {
    def setKeyDown(n: N, f: KeyboardEvent => Unit): Unit
    def setKeyPress(n: N, f: KeyboardEvent => Unit): Unit
    def setKeyUp(n: N, f: KeyboardEvent => Unit): Unit
  }
  object BindTo {

    implicit val bindToWindow: BindTo[Window] =
      new BindTo[Window] {
        override def setKeyDown(n: Window, f: KeyboardEvent => Unit): Unit = n.onkeydown = f
        override def setKeyPress(n: Window, f: KeyboardEvent => Unit): Unit = n.onkeypress = f
        override def setKeyUp(n: Window, f: KeyboardEvent => Unit): Unit = n.onkeyup = f
      }

  }

}
