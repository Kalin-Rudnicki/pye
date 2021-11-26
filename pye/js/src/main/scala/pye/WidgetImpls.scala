package pye

import monocle.Lens
import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.fp.Implicits._
import klib.fp.types._
import klib.fp.utils._
import klib.utils._

private[pye] object WidgetImpls {

  object widgets {

    // TODO (KR) : Name
    // =====| Core |=====

    def leaf[V, S, A](
        elementF: RaiseHandler[S, A] => S => Widget.ElementT,
        valueF: S => ?[V],
    ): Widget[V, S, A] =
      new Widget[V, S, A] {

        override final val widgetName: String = "Leaf"

        override protected final def convertImpl(
            parentRaiseHandler: RaiseHandler[S, A],
            getState: () => S,
        ): AppliedWidget[V] =
          new AppliedWidget[V] {
            private val elems: Var[Maybe[Widget.ElementT]] = Var(None)

            override final val valueImpl: IO[V] =
              IO.wrapEffect { valueF(getState()) }
            override final val currentImpl: IO[Maybe[Widget.ElementT]] =
              IO { elems.value }
            override final val getElementsAndUpdateImpl: IO[Widget.ElementT] =
              for {
                newElems <- IO { elementF(parentRaiseHandler)(getState()) }
                _ <- elems.value = newElems.some
              } yield newElems
          }

      }

    def mapV[V0, V, S, A](
        w: Widget[V0, S, A],
        f: (S, ?[V0]) => ?[V],
    ): Widget[V, S, A] =
      new Widget[V, S, A] {

        override final val widgetName: String = "MapV"

        override protected final def convertImpl(
            parentRaiseHandler: RaiseHandler[S, A],
            getState: () => S,
        ): AppliedWidget[V] = {
          val child: AppliedWidget[V0] = w.convert(parentRaiseHandler, getState)

          new AppliedWidget[V] {
            override final val valueImpl: IO[V] =
              IO.wrapEffect { f(getState(), child.value.runSync) }
            override final val currentImpl: IO[Maybe[Widget.ElementT]] =
              child.current
            override final val getElementsAndUpdateImpl: IO[Widget.ElementT] =
              child.getElementsAndUpdate
          }
        }

      }

    def apply[V0, V, S, A](
        _w1: Widget[V0, S, A],
        _w2: Widget[V0 => V, S, A],
    ): Widget[V, S, A] =
      new Widget[V, S, A] {

        override final val widgetName: String = "Apply"

        override protected final def convertImpl(
            parentRaiseHandler: RaiseHandler[S, A],
            getState: () => S,
        ): AppliedWidget[V] = {
          val w1: AppliedWidget[V0] =
            _w1.captureReRender.convert(parentRaiseHandler, getState)
          val w2: AppliedWidget[V0 => V] =
            _w2.captureReRender.convert(parentRaiseHandler, getState)

          new AppliedWidget[V] {
            override final val valueImpl: IO[V] =
              w1.value.apply(w2.value)
            override final val currentImpl: IO[Maybe[Widget.ElementT]] =
              ado[MaybeMonad.Projection[IO]#P]
                .join(
                  w1.current.toMaybeMonad,
                  w2.current.toMaybeMonad,
                )
                .map {
                  case (w1E, w2E) =>
                    NonEmptyList.nel(w1E, w2E).flatten
                }
                .wrapped
            override final val getElementsAndUpdateImpl: IO[Widget.ElementT] =
              ado[IO]
                .join(
                  w1.getElementsAndUpdate,
                  w2.getElementsAndUpdate,
                )
                .map {
                  case (w1E, w2E) =>
                    NonEmptyList.nel(w1E, w2E).flatten
                }
          }
        }

      }

    def mapA[A0, V, S, A](
        w: Widget[V, S, A0],
        f: (S, ?[V], Raise[S, A0]) => AsyncIO[List[Raise[S, A]]],
    ): Widget[V, S, A] =
      new Widget[V, S, A] {

        override final val widgetName: String = "MapA"

        override protected final def convertImpl(
            parentRaiseHandler: RaiseHandler[S, A],
            getState: () => S,
        ): AppliedWidget[V] =
          Pointer
            .withSelf[AppliedWidget[V]] { ptr =>
              val mappedRH: RaiseHandler[S, A0] = { raise =>
                for {
                  raises <- f(getState(), ptr.value.value.runSync, raise)
                  prr <- parentRaiseHandler._handleRaises(raises)
                } yield prr
              }

              Pointer(w.convert(mappedRH, getState))
            }
            .value

      }

    def flatMap[V0, V, S, A](
        _w1: Widget[V0, S, A],
        _w2: V0 => Widget[V, S, A],
        w2GoesAfter: Boolean,
    ): Widget[V, S, A] =
      new Widget[V, S, A] {

        override final val widgetName: String = "FlatMap"

        override protected final def convertImpl(
            parentRaiseHandler: RaiseHandler[S, A],
            getState: () => S,
        ): AppliedWidget[V] = {
          def makeW2(v1: V0): AppliedWidget[V] =
            _w2(v1).captureReRender.convert(parentRaiseHandler, getState)

          lazy val w1: AppliedWidget[V0] =
            _w1.captureReRender(RaiseHandler.ReRender(w2)).convert(parentRaiseHandler, getState)

          lazy val w2: AppliedWidget[V] =
            new AppliedWidget[V] {
              private val inner: Var[Maybe[Widget.ElementT] \/ AppliedWidget[V]] = Var(None.left)

              override final val valueImpl: IO[V] = {
                for {
                  // TODO (KR) : There is a bug here...
                  //           : Somehow adding this blank `IO {}` fixes it
                  //           : (there are 2 of these)
                  _ <- IO {}

                  v1 <- w1.value
                  _w2 <- IO { makeW2(v1) }
                  v2 <- _w2.value
                } yield v2
              }
              override final val currentImpl: IO[Maybe[Widget.ElementT]] = {
                for {
                  evalInner <- IO { inner.value }
                  elems <- evalInner match {
                    case Right(w) => w.current
                    case Left(e)  => IO { e }
                  }
                } yield elems
              }
              override final val getElementsAndUpdateImpl: IO[Widget.ElementT] = {
                for {
                  v1_? <- IO { w1.value.runSync }
                  elems <- v1_? match {
                    case Alive(r) =>
                      for {
                        w2 <- IO { makeW2(r) }
                        newElems <- w2.getElementsAndUpdate
                        _ <- inner.value = w2.right
                      } yield newElems
                    case Dead(_) =>
                      for {
                        newElem <- IO { Widget.placeholderSpan }
                        newElems = NonEmptyList.nel(newElem)
                        _ <- inner.value = newElems.some.left
                      } yield newElems
                  }
                } yield elems
              }
            }

          /*
            NOTE : Current approach is not going to work
                 : There is no way to have the second part of flatMap know how to reRender
           */

          new AppliedWidget[V] {
            override final val valueImpl: IO[V] =
              w2.value
            override final val currentImpl: IO[Maybe[Widget.ElementT]] =
              ado[MaybeMonad.Projection[IO]#P]
                .join(
                  w1.current.toMaybeMonad,
                  w2.current.toMaybeMonad,
                )
                .map {
                  case (w1E, w2E) =>
                    NonEmptyList.nel(w1E, w2E).flatten
                }
                .wrapped
            override final val getElementsAndUpdateImpl: IO[Widget.ElementT] =
              ado[IO]
                .join(
                  w1.getElementsAndUpdate,
                  w2.getElementsAndUpdate,
                )
                .map {
                  case (w1E, w2E) =>
                    val (elems1, elems2) =
                      if (w2GoesAfter) (w1E, w2E)
                      else (w2E, w1E)

                    NonEmptyList.nel(elems1, elems2).flatten
                }
          }
        }

      }

    def wrapped[V, S, A](
        w: Widget[V, S, A],
        f: RaiseHandler[S, A] => S => Modifier => Widget.ElementT,
    ): Widget[V, S, A] =
      new Widget[V, S, A] {

        override final val widgetName: String = "Wrapped"

        override protected final def convertImpl(
            parentRaiseHandler: RaiseHandler[S, A],
            getState: () => S,
        ): AppliedWidget[V] = {
          val child: AppliedWidget[V] =
            w.captureReRender.convert(parentRaiseHandler, getState)

          new AppliedWidget[V] {
            private val elems: Var[Maybe[Widget.ElementT]] = Var(None)

            override final val valueImpl: IO[V] =
              child.value
            override final val currentImpl: IO[Maybe[Widget.ElementT]] =
              IO { elems.value }
            override final val getElementsAndUpdateImpl: IO[Widget.ElementT] =
              for {
                childElems <- child.getElementsAndUpdate
                myElems = f(parentRaiseHandler)(getState())(childElems.toList)
                _ <- elems.value = myElems.some
              } yield myElems
          }
        }

      }

    def zoomOut[S0, V, S, A](
        w: Widget[V, S, A],
        lens: Lens[S0, S],
    ): Widget[V, S0, A] =
      new Widget[V, S0, A] {

        override final val widgetName: String = "ZoomOut"

        override protected final def convertImpl(
            parentRaiseHandler: RaiseHandler[S0, A],
            getState: () => S0,
        ): AppliedWidget[V] = {
          val rh: RaiseHandler[S, A] = {
            case sou: Raise.StandardOrUpdate[S] =>
              sou match {
                case update: Raise.UpdateState[S] =>
                  parentRaiseHandler._handleRaise(
                    update.mapUpdate[S0] { uF => lens.modify(uF) },
                  )
                case standard: Raise.Standard =>
                  parentRaiseHandler._handleRaise(standard)
              }
            case action: Raise.Action[A] =>
              parentRaiseHandler._handleRaise(action)
          }

          w.convert(rh, () => lens.get(getState()))
        }

      }

    // =====| TypeClass |=====

    def traverseList[T, S, A](
        t: List[Widget[T, S, A]],
    ): Widget[List[T], S, A] =
      new Widget[List[T], S, A] {

        override final val widgetName: String = "TraverseList"

        override protected def convertImpl(
            parentRaiseHandler: RaiseHandler[S, A],
            getState: () => S,
        ): AppliedWidget[List[T]] =
          t.toNel match {
            case Some(nel) =>
              type MyT[V] = Widget[V, S, A]
              (nel: NonEmptyList[MyT[T]]).traverse.map(_.toList).convert(parentRaiseHandler, getState)
            case None =>
              new AppliedWidget[List[T]] {
                private val elems: Var[Maybe[Widget.ElementT]] = Var(None)

                // TODO (KR) : This seems wrong...
                override final val valueImpl: IO[List[T]] = IO { Nil }
                override final val currentImpl: IO[Maybe[Widget.ElementT]] = IO { elems.value }
                override final val getElementsAndUpdateImpl: IO[Widget.ElementT] =
                  for {
                    newElems <- IO { NonEmptyList.nel(Widget.placeholderSpan) }
                    _ <- elems.value = newElems.some
                  } yield newElems
              }
          }

      }

    def traverseNonEmptyList[T, S, A](
        t: NonEmptyList[Widget[T, S, A]],
    ): Widget[NonEmptyList[T], S, A] =
      new Widget[NonEmptyList[T], S, A] {

        override final val widgetName: String = "TraverseNonEmptyList"

        override protected final def convertImpl(
            parentRaiseHandler: RaiseHandler[S, A],
            getState: () => S,
        ): AppliedWidget[NonEmptyList[T]] = {
          val children: NonEmptyList[AppliedWidget[T]] =
            t.map(_.captureReRender.convert(parentRaiseHandler, getState))

          new AppliedWidget[NonEmptyList[T]] {

            override final val valueImpl: IO[NonEmptyList[T]] = children.map(_.value).traverse
            override final val currentImpl: IO[Maybe[Widget.ElementT]] =
              children
                .map(_.current)
                .traverse
                .map {
                  _.traverse
                    .map(_.flatten)
                }
            override final val getElementsAndUpdateImpl: IO[Widget.ElementT] =
              children.map(_.getElementsAndUpdate).traverse.map(_.flatten)
          }
        }

      }

    // =====| ReRender |=====

    def captureReRender[V, S, A](
        w: Widget[V, S, A],
        reRenders: => RaiseHandler.ReRender,
    ): Widget[V, S, A] =
      new Widget[V, S, A] {
        override val widgetName: String = s"${w.widgetName}-captureReRender"
        override protected def convertImpl(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V] =
          Pointer
            .withSelf[AppliedWidget[V]] { ptr =>
              val rh: RaiseHandler[S, A] = helpers.rhCaptureReRender(ptr, parentRaiseHandler, reRenders)
              Pointer(w.convert(rh, getState))
            }
            .value
      }

    def captureTags[V, S, A](
        w: Widget[V, S, A],
        tags: Set[String],
    ): Widget[V, S, A] =
      new Widget[V, S, A] {
        override final val widgetName: String = s"${w.widgetName}-captureTags"
        override final protected def convertImpl(
            parentRaiseHandler: RaiseHandler[S, A],
            getState: () => S,
        ): AppliedWidget[V] =
          Pointer
            .withSelf[AppliedWidget[V]] { ptr =>
              val rh: RaiseHandler[S, A] = helpers.rhCaptureTags(ptr, parentRaiseHandler, tags)
              Pointer(w.convert(rh, getState))
            }
            .value
      }

    def joinWithReRenderLogic[V1, V2, V, S, A](
        w1: Widget[V1, S, A],
        w2: Widget[V2, S, A],
        w1ReRendersW2: Boolean,
        w2ReRendersW1: Boolean,
        w1BeforeW2: Boolean,
        mapV: (V1, V2) => V,
    ): Widget[V, S, A] =
      new Widget[V, S, A] {

        override final val widgetName: String = "JoinWithReRenderLogic"

        override protected def convertImpl(
            parentRaiseHandler: RaiseHandler[S, A],
            getState: () => S,
        ): AppliedWidget[V] = {
          def buildAppliedWidget[WV, AWV](
              w: Widget[WV, S, A],
              aw: => AppliedWidget[AWV],
              wReRendersAW: Boolean,
          ): AppliedWidget[WV] =
            w.captureReRender(wReRendersAW ? RaiseHandler.ReRender(aw) | RaiseHandler.ReRender.Nothing)
              .convert(parentRaiseHandler, getState)

          lazy val aW1: AppliedWidget[V1] = buildAppliedWidget(w1, aW2, w1ReRendersW2)
          lazy val aW2: AppliedWidget[V2] = buildAppliedWidget(w2, aW1, w2ReRendersW1)

          new AppliedWidget[V] {
            override protected val valueImpl: IO[V] =
              ado[IO].join(aW1.value, aW2.value).map(mapV.tupled)

            override protected val currentImpl: IO[Maybe[Widget.ElementT]] =
              ado[IO]
                .join(aW1.current, aW2.current)
                .flatMap {
                  case (Some(mE1), Some(mE2)) => (w1BeforeW2 ? (mE1 ::: mE2) | (mE2 ::: mE1)).some.pure[IO]
                  case (None, None)           => None.pure[IO]
                  case _                      => IO.error(Message("Current in bad state"))
                }

            override protected val getElementsAndUpdateImpl: IO[Widget.ElementT] =
              ado[IO]
                .join(aW1.getElementsAndUpdate, aW2.getElementsAndUpdate)
                .flatMap { case (e1, e2) => (w1BeforeW2 ? (e1 ::: e2) | (e2 ::: e1)).pure[IO] }

          }
        }

      }

    // =====| Misc |=====

    def logRaises[V, S, A](
        w: Widget[V, S, A],
        label: String,
    ): Widget[V, S, A] =
      new Widget[V, S, A] {

        override final val widgetName: String = "logRaises"
        override protected def convertImpl(parentRaiseHandler: RaiseHandler[S, A], getState: () => S): AppliedWidget[V] =
          new AppliedWidget[V] {
            private val child: AppliedWidget[V] =
              w.convert(
                r =>
                  for {
                    _ <- PyeLogger.log.debug(s"logRaises [$label]", "raise").toAsyncIO
                    res <- parentRaiseHandler._handleRaise(r)
                  } yield res,
                getState,
              )

            override final protected val valueImpl: IO[V] = child.value
            override final protected val currentImpl: IO[Maybe[Widget.ElementT]] = child.current
            override final protected val getElementsAndUpdateImpl: IO[pye.Widget.ElementT] = child.getElementsAndUpdate
          }
      }

    // =====| Template |=====

    /*
    def todoName[V, S, A](
        w: Widget[V, S, A],
    ): Widget[V, S, A] =
      new Widget[V, S, A] {}
     */

  }

  object helpers {

    def rhCaptureReRender[V, S, A](
        w: Pointer[AppliedWidget[V]],
        parentRH: RaiseHandler[S, A],
        reRenders: => RaiseHandler.ReRender = RaiseHandler.ReRender.Nothing,
    ): RaiseHandler[S, A] = {
      case sou: Raise.StandardOrUpdate[S] =>
        sou match {
          case standard: Raise.Standard =>
            parentRH._handleRaise(standard)
          case update: Raise.UpdateState[S] =>
            val (usRR, rhRR) = update.reRender.ifForced(w.value, update.childReRenders)

            parentRH._handleRaise(
              Raise.UpdateState[S](
                update.update,
                usRR,
                RaiseHandler.ReRender.merge(
                  List(
                    reRenders,
                    rhRR,
                  ),
                ),
              ),
            )
        }
      case action: Raise.Action[A] =>
        parentRH._handleRaise(action)
    }

    def rhCaptureTags[V, S, A](
        w: Pointer[AppliedWidget[V]],
        parentRH: RaiseHandler[S, A],
        tags: Set[String],
    ): RaiseHandler[S, A] = {
      case sou: Raise.StandardOrUpdate[S] =>
        sou match {
          case standard: Raise.Standard =>
            parentRH._handleRaise(standard)
          case update: Raise.UpdateState[S] =>
            val (usRR, rhRR) = update.reRender.ifTagged(tags, w.value, update.childReRenders)

            PyeLogger.unsafeLog.debug(s"<Tag: ${tags.mkString(", ")}>\n${update.childReRenders}\n$rhRR", "raise")

            parentRH._handleRaise(
              Raise.UpdateState[S](
                update.update,
                usRR,
                rhRR,
              ),
            )
        }
      case action: Raise.Action[A] =>
        parentRH._handleRaise(action)
    }

  }

}
