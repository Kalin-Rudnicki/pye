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
          private val children: Var[List[AppliedWidget[V]]] = Var(Nil)

          override protected val valueImpl: IO[List[V]] =
            for {
              c <- IO { children.value }
              v <- c.map(_.value).traverse
            } yield v

          override protected val currentImpl: IO[Maybe[Widget.ElementT]] =
            for {
              c <- IO { children.value }
              l <- c.map(_.current).traverse
            } yield l.flatMap(_.map(_.toList).toList.flatten).toNel

          override protected val getElementsAndUpdateImpl: IO[Widget.ElementT] =
            for {
              sl <- IO { getState() }
              aw = sl.zipWithIndex.map {
                case (_, i) =>
                  widget.captureReRender.convert(
                    r =>
                      parentRaiseHandler._handleRaise(
                        r match {
                          case update: Raise.UpdateState[S] =>
                            update.mapUpdate[List[S]] { updateS => listS =>
                              listS.zipWithIndex.map { case (s2, i2) => (i == i2) ? updateS(s2) | s2 }
                            }
                          case standard: Raise.Standard => standard
                          case Raise.Action(action)     => Raise.Action(KeyedAction(i, action))
                        },
                      ),
                    // TODO (KR) : Not sure which to use (?)
                    () => getState()(i),
                    // () => s,
                  )
              }
              e1 <- aw.map(_.getElementsAndUpdate).traverse
              e2 = e1.flatMap(_.toList).toNel match {
                case Some(elems) => elems
                case None        => NonEmptyList.nel(Widget.placeholderSpan)
              }
              _ <- children.value = aw
            } yield e2

        }

    }

  // TODO (KR) : I believe if `S` changes for 1 entry, the whole widget would need to re-render
  //           : (Because K is a function of S)
  def mapW[V, S, A, K](
      widget: Widget[V, S, A],
      stateToKey: S => K,
      ordering: Ordering[K],
  ): Widget[List[V], List[S], KeyedAction[K, A]] = {
    // TODO (KR) :
    ???
  }

}
object multi extends multi
