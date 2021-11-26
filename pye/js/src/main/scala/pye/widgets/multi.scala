package pye.widgets

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import pye._

trait multi {

  final case class KeyedAction[+K, +A](
      key: K,
      action: A,
  )
  object KeyedAction {
    type List[S, A] = KeyedAction[(S, Int), A]
    type ListU[S] = KeyedAction.List[S, Unit]
  }

  sealed trait RemoveOr[+A]
  object RemoveOr {
    case object Remove extends RemoveOr[Nothing]
    final case class Or[+A](action: A) extends RemoveOr[A]
  }

  def listW[V, S, A](widget: Widget[V, S, A]): Widget[List[V], List[S], KeyedAction[(S, Int), A]] =
    new Widget[List[V], List[S], KeyedAction[(S, Int), A]] {

      override val widgetName: String = s"ListW[${widget.widgetName}]"

      override protected def convertImpl(
          parentRaiseHandler: RaiseHandler[List[S], KeyedAction[(S, Int), A]],
          getState: () => List[S],
      ): AppliedWidget[List[V]] =
        new AppliedWidget[List[V]] {
          private type State = Widget.ElementT \/ NonEmptyList[AppliedWidget[V]]

          private val children: Var[State] =
            Var(NonEmptyList.nel(Widget.placeholderSpan).left)

          override protected val valueImpl: IO[List[V]] =
            for {
              c <- IO { children.value }
              v <- c match {
                case Right(c) => c.map(_.value).traverse.map(_.toList)
                case Left(_)  => Nil.pure[IO]
              }
            } yield v

          override protected val currentImpl: IO[Maybe[Widget.ElementT]] =
            for {
              c <- IO { children.value }
              e <- c match {
                case Right(c) =>
                  for {
                    l <- c.map(_.current).traverse
                    e <- l.toList.flatMap(_.map(_.toList).toList.flatten).toNelIO("List widget is in bad state")
                  } yield e.some
                case Left(e) => e.some.pure[IO]
              }
            } yield e

          override protected val getElementsAndUpdateImpl: IO[Widget.ElementT] = {
            def calcForSome(sl: NonEmptyList[S]): IO[(State, Widget.ElementT)] = {
              def convertRaise(s: S, i: Int, r: Raise[S, A]): Raise[List[S], KeyedAction[(S, Int), A]] =
                r match {
                  case update: Raise.UpdateState[S] =>
                    update.mapUpdate[List[S]] { updateS => listS =>
                      listS.zipWithIndex.map { case (s2, i2) => (i == i2) ? updateS(s2) | s2 }
                    }
                  case standard: Raise.Standard => standard
                  case Raise.Action(action)     => Raise.Action(KeyedAction((s, i), action))
                }

              val aw =
                sl.zipWithIndex.map {
                  case (s, i) =>
                    widget.captureReRender.convert(
                      r => parentRaiseHandler._handleRaise(convertRaise(s, i, r)),
                      // TODO (KR) : Not sure which to use (?)
                      () => getState()(i),
                      // () => s,
                    )
                }

              for {
                e <- aw.map(_.getElementsAndUpdate).traverse
              } yield (
                aw.right,
                e.flatten,
              )
            }

            def calcForNone: IO[(State, Widget.ElementT)] =
              IO {
                val e = NonEmptyList.nel(Widget.placeholderSpan)
                (
                  e.left,
                  e,
                )
              }

            for {
              sl <- IO { getState() }
              tmp1 <- sl.toNel match {
                case Some(sl) => calcForSome(sl)
                case None     => calcForNone
              }
              (newChildren, elems) = tmp1
              _ <- children.value = newChildren
            } yield elems
          }

        }

    }

  def removableListW[V, S, A](widget: Widget[V, S, RemoveOr[A]]): Widget[List[V], List[S], KeyedAction[(S, Int), A]] =
    listW(widget)
      .covariantMapAction[KeyedAction[(S, Int), RemoveOr[A]], KeyedAction[(S, Int), A]] {
        case (_, _, KeyedAction((s, idx), action)) =>
          action match {
            case RemoveOr.Remove =>
              AsyncIO { Raise.updateState[List[S]](_.zipWithIndex.filterNot(_._2 == idx).map(_._1)) :: Nil }
            case RemoveOr.Or(action) =>
              AsyncIO { Raise.Action(KeyedAction((s, idx), action)) :: Nil }
          }
      }

}
object multi extends multi
