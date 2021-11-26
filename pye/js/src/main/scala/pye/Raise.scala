package pye

import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import pye.Implicits._
import pye.widgets.modifiers._

sealed trait Raise[+S, +A] {

  def mapAction[A2](f: A => A2): Raise[S, A2] =
    this match {
      case Raise.Action(action)           => Raise.Action(f(action))
      case sou: Raise.StandardOrUpdate[S] => sou
    }

}
object Raise {
  sealed trait StandardOrUpdate[+S] extends Raise[S, Nothing]

  final case class UpdateState[S](
      update: S => S,
      reRender: UpdateState.ReRender, // = UpdateState.ReRender.Force,
      childReRenders: RaiseHandler.ReRender, // = RaiseHandler.ReRender.Nothing,
  ) extends StandardOrUpdate[S] {

    def mapUpdate[S2](f: (S => S) => (S2 => S2)): UpdateState[S2] =
      UpdateState[S2](
        update = f(update),
        reRender = reRender,
        childReRenders = childReRenders,
      )

    def propagate: UpdateState[S] =
      withReRender(UpdateState.ReRender.Propagate)

    def tagged(tag: String): UpdateState[S] =
      withReRender(UpdateState.ReRender.Tag(tag))

    def withReRender(reRender: UpdateState.ReRender): UpdateState[S] =
      UpdateState(update, reRender, childReRenders)

  }
  object UpdateState {

    sealed trait ReRender {

      def ifForced(
          aw: AppliedWidget[_],
          child: RaiseHandler.ReRender,
      ): (Raise.UpdateState.ReRender, RaiseHandler.ReRender) =
        this match {
          case ReRender.Propagate => (ReRender.Propagate, child)
          case ReRender.Force     => (ReRender.Propagate, RaiseHandler.ReRender(aw))
          case tag: ReRender.Tag  => (tag, child)
        }

      def ifTagged(
          tags: Set[String],
          aw: AppliedWidget[_],
          child: RaiseHandler.ReRender,
      ): (Raise.UpdateState.ReRender, RaiseHandler.ReRender) =
        this match {
          case ReRender.Propagate =>
            (ReRender.Propagate, child)
          case ReRender.Force =>
            (ReRender.Force, child)
          case tag: ReRender.Tag =>
            if (tags.contains(tag.tag)) (ReRender.Propagate, RaiseHandler.ReRender(aw))
            else (tag, child)
        }

    }
    object ReRender {
      case object Propagate extends ReRender
      case object Force extends ReRender
      final case class Tag(tag: String) extends ReRender
    }

  }

  def setState[S](s: => S): UpdateState[S] =
    updateState[S] { _ => s }

  def updateState[S](f: S => S): UpdateState[S] =
    UpdateState[S](f, UpdateState.ReRender.Force, RaiseHandler.ReRender.Nothing)

  sealed trait Standard extends StandardOrUpdate[Nothing]
  final case class DisplayMessage(
      message: String,
      modifier: Modifier,
      timeout: Maybe[Int],
      causeId: Maybe[String],
  ) extends Standard
  object DisplayMessage {
    final class Builder private[DisplayMessage] (causeId: Maybe[String]) {

      def apply(
          message: String,
          timeout: Maybe[Int] = None,
          modifier: Modifier = Seq.empty[Modifier],
      ): DisplayMessage =
        DisplayMessage(
          message = message,
          timeout = timeout,
          modifier = modifier,
          causeId = causeId,
        )

      def info(
          message: String,
          timeout: Maybe[Int] = None,
          decorator: Modifier = Seq.empty[Modifier],
      ): DisplayMessage =
        apply(
          message = message,
          timeout = timeout,
          modifier = Seq[Modifier](PyeS.message.m(_.info), decorator),
        )

      def error(
          message: String,
          timeout: Maybe[Int] = None,
          decorator: Modifier = Seq.empty[Modifier],
      ): DisplayMessage =
        apply(
          message = message,
          timeout = timeout,
          modifier = Seq[Modifier](PyeS.message.m(_.error), decorator),
        )

    }

    def fromThrowable(throwable: Throwable): DisplayMessage =
      global.error(
        Maybe(throwable.getMessage).getOrElse(throwable.toString),
        decorator = Seq(
          oncontextmenu := { (e: Event) =>
            e.preventDefault()
            console.log(throwableSourceMapReference(throwable).toString("    "))
          },
        ),
      )

    val global: Builder = new Builder(None)
    def forId(id: String): Builder = new Builder(id.some)
  }
  sealed trait History extends Standard
  object History {
    final case class Push(page: () => Page) extends History
    final case class Replace(page: () => Page) extends History
    final case class Go(delta: Int) extends History

    def push(page: => Page): Push = Push(() => page)
    def replace(page: => Page): Replace = Replace(() => page)

    val forward: Go = Go(1)
    val back: Go = Go(-1)
  }
  case object RefreshPage extends Standard
  final case class Raw(action: AsyncIO[Unit], hint: Maybe[String] = None) extends Standard {
    def withHint(hint: String): Raw = Raw(action, hint.some)
    override def toString: String = s"Raw(${hint.cata(identity, "")})"
  }

  final case class Action[+A](action: A) extends Raise[Nothing, A]
}
