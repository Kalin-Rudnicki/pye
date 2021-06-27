package klib.webServer

import scala.annotation.tailrec

import org.scalajs.dom._

import klib.Implicits._
import klib.fp.types._

final class KeyMap {
  import scala.collection.mutable

  // TODO (KR) : Add a binding for displaying keymap, or at least logging it...
  private val onDownKeys: mutable.ListBuffer[KeyMap.Key] = mutable.ListBuffer()
  private val onPressKeys: mutable.ListBuffer[KeyMap.Key] = mutable.ListBuffer()
  private val onUpKeys: mutable.ListBuffer[KeyMap.Key] = mutable.ListBuffer()

  private[webServer] def bindToWindow(): Unit = {
    def buildListener(lb: mutable.ListBuffer[KeyMap.Key])(e: KeyboardEvent): Unit = {
      @tailrec
      def loop(keys: List[KeyMap.Key]): Unit =
        keys match {
          case head :: tail =>
            if (head.matches(e)) {
              if (head.preventsDefault)
                e.preventDefault()
              head.action(e)
            } else
              loop(tail)
          case Nil =>
        }

      loop(lb.toList)
    }

    window.onkeydown = buildListener(onDownKeys)(_)
    window.onkeypress = buildListener(onPressKeys)(_)
    window.onkeyup = buildListener(onUpKeys)(_)
  }

  // =====|  |=====

  def on(key: KeyMap.Key): this.type = {
    {
      key.on match {
        case KeyMap.On.KeyDown  => onDownKeys
        case KeyMap.On.KeyPress => onPressKeys
        case KeyMap.On.KeyUp    => onUpKeys
      }
    }.append(key)

    this
  }

  def onDown(
      key: String,
      name: String,
      ctrl: Maybe[Boolean] = false.some,
      shift: Maybe[Boolean] = false.some,
      preventsDefault: Boolean = true,
  )(action: KeyboardEvent => Unit): this.type =
    on(
      KeyMap.Key(
        keys = Set(key),
        name = name,
        action = action,
        ctrl = ctrl,
        shift = shift,
        on = KeyMap.On.KeyDown,
        preventsDefault = preventsDefault,
      ),
    )

  def onPress(
      key: String,
      name: String,
      ctrl: Maybe[Boolean] = false.some,
      shift: Maybe[Boolean] = false.some,
      preventsDefault: Boolean = true,
  )(action: KeyboardEvent => Unit): this.type =
    on(
      KeyMap.Key(
        keys = Set(key),
        name = name,
        action = action,
        ctrl = ctrl,
        shift = shift,
        on = KeyMap.On.KeyPress,
        preventsDefault = preventsDefault,
      ),
    )

  def onUp(
      key: String,
      name: String,
      ctrl: Maybe[Boolean] = false.some,
      shift: Maybe[Boolean] = false.some,
      preventsDefault: Boolean = true,
  )(action: KeyboardEvent => Unit): this.type =
    on(
      KeyMap.Key(
        keys = Set(key),
        name = name,
        action = action,
        ctrl = ctrl,
        shift = shift,
        on = KeyMap.On.KeyUp,
        preventsDefault = preventsDefault,
      ),
    )

}
object KeyMap {

  final case class Key(
      keys: Set[String],
      name: String,
      action: KeyboardEvent => Unit,
      ctrl: Maybe[Boolean],
      shift: Maybe[Boolean],
      preventsDefault: Boolean,
      on: On,
  ) {

    def matches(e: KeyboardEvent): Boolean =
      keys.contains(e.key) &&
        ctrl.cata(_ == e.ctrlKey, true) &&
        shift.cata(_ == e.shiftKey, true)

  }

  sealed trait On
  object On {
    case object KeyDown extends On
    case object KeyPress extends On
    case object KeyUp extends On
  }

}
