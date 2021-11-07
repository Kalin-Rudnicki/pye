package pye.widgets

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import pye._

trait multi {

  final case class KeyedAction[K, A](
      key: K,
      action: A,
  )

  def listW[V, S, A](widget: Widget[V, S, A]): Widget[List[V], List[S], KeyedAction[Int, A]] =
    new Widget[List[V], List[S], KeyedAction[Int, A]] {

      override val widgetName: String = s"ListW[${widget.widgetName}]"

      override protected def convertImpl(
          parentRaiseHandler: RaiseHandler[List[S], KeyedAction[Int, A]],
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
              def convertRaise(i: Int, r: Raise[S, A]): Raise[List[S], KeyedAction[Int, A]] =
                r match {
                  case update: Raise.UpdateState[S] =>
                    update.mapUpdate[List[S]] { updateS => listS =>
                      listS.zipWithIndex.map { case (s2, i2) => (i == i2) ? updateS(s2) | s2 }
                    }
                  case standard: Raise.Standard => standard
                  case Raise.Action(action)     => Raise.Action(KeyedAction(i, action))
                }

              val aw =
                sl.zipWithIndex.map {
                  case (_, i) =>
                    widget.captureReRender.convert(
                      r => parentRaiseHandler._handleRaise(convertRaise(i, r)),
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

  // TODO (KR) : I believe if `S` changes for 1 entry, the whole widget would need to re-render
  //           : (Because K is a function of S)
  def mapW[K, V, S, A](
      widget: Widget[V, S, A],
      stateToKey: S => K,
  )(implicit
      widgetOrdering: Ordering[K],
  ): Widget[Map[K, V], List[S], KeyedAction[K, A]] =
    new Widget[Map[K, V], List[S], KeyedAction[K, A]] {

      override val widgetName: String = s"MapW[${widget.widgetName}]"

      override protected def convertImpl(
          parentRaiseHandler: RaiseHandler[List[S], KeyedAction[K, A]],
          getState: () => List[S],
      ): AppliedWidget[Map[K, V]] =
        new AppliedWidget[Map[K, V]] {
          private type State = Widget.ElementT \/ Map[K, AppliedWidget[V]]

          private val children: Var[State] =
            Var(NonEmptyList.nel(Widget.placeholderSpan).left)

          override protected val valueImpl: IO[Map[K, V]] =
            for {
              c <- IO { children.value }
              v <- c match {
                case Right(c) =>
                  c.toList
                    .map {
                      case (k, aw) =>
                        aw.value.map((k, _))
                    }
                    .traverse
                    .map(_.toMap)
                case Left(_) =>
                  Map.empty[K, V].pure[IO]
              }
            } yield v

          override protected val currentImpl: IO[Maybe[Widget.ElementT]] =
            for {
              c <- IO { children.value }
              e <- c match {
                case Right(c) =>
                  for {
                    c2 <- c.toList.toNelIO("Map widget is in a bad state (1)")
                    l <- c2.map {
                      case (k, aw) =>
                        for {
                          e1 <- aw.current
                          e2 <- e1.toEA(Message("Map widget is in a bad state (2)")).toIO
                        } yield (k, e2)
                    }.traverse
                    e = l.sortBy(_._1)(widgetOrdering).map(_._2).flatten
                  } yield e.some
                case Left(e) => e.some.pure[IO]
              }
            } yield e

          override protected val getElementsAndUpdateImpl: IO[Widget.ElementT] = {
            def calcForSome(sl: NonEmptyList[S]): IO[(State, Widget.ElementT)] = {
              def convertRaise(i: Int, r: Raise[S, A]): Raise[List[S], KeyedAction[Int, A]] =
                r match {
                  case update: Raise.UpdateState[S] =>
                    update.mapUpdate[List[S]] { updateS => listS =>
                      listS.zipWithIndex.map { case (s2, i2) => (i == i2) ? updateS(s2) | s2 }
                    }
                  case standard: Raise.Standard => standard
                  case Raise.Action(action)     => Raise.Action(KeyedAction(i, action))
                }

              val aw =
                sl.zipWithIndex.map {
                  case (_, i) =>
                    widget.captureReRender.convert(
                      r => parentRaiseHandler._handleRaise(convertRaise(i, r)),
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

}
object multi extends multi
