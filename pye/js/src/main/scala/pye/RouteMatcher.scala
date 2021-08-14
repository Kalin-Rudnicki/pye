package pye

import scala.annotation.tailrec

import org.scalajs.dom._
import org.scalajs.dom.experimental.URLSearchParams

import klib.Implicits._
import klib.fp.typeclass.DecodeString
import klib.fp.types._
import klib.utils._
import pye.Implicits._

sealed trait RouteMatcher {

  final def /:(const: String): RouteMatcher =
    RouteMatcher.const(const)(this)

  final def toIdtString: IndentedString = {
    import IndentedString._

    this match {
      case oneOf: RouteMatcher.OneOf =>
        inline(
          "one-of:",
          indented(
            oneOf.children.map(_.toIdtString),
          ),
        )
      case const: RouteMatcher.Const =>
        inline(
          const.const,
          indented(
            const.child.toIdtString,
          ),
        )
      case _: RouteMatcher.Complete =>
        "complete"
      case pathArg: RouteMatcher.PathArg[_] =>
        inline(
          "path-arg",
          indented(
            pathArg.child(null.asInstanceOf[pathArg.Type]).toIdtString,
          ),
        )
    }
  }

  private[pye] final def attemptToLoadPage(): Unit = {
    val paths =
      window.location.pathname
        .split("/")
        .toList
        .filter(_.nonEmpty)
    val params =
      RouteMatcher.Params {
        new URLSearchParams(window.location.search).toList.map { t => (t._1, t._2) }.toMap
      }

    def oneOfMatch(paths: List[String], oneOf: RouteMatcher.OneOf): Maybe[Page] = {
      @tailrec
      def loop(children: List[RouteMatcher]): Maybe[Page] =
        children match {
          case cHead :: cTail =>
            attemptMatch(paths, cHead) match {
              case None           => loop(cTail)
              case some @ Some(_) => some
            }
          case Nil =>
            None
        }

      loop(oneOf.children)
    }

    // TODO (KR) :
    @tailrec
    def attemptMatch(
        paths: List[String],
        routeMatcher: RouteMatcher,
    ): Maybe[Page] =
      paths match {
        case pHead :: pTail =>
          routeMatcher match {
            case oneOf: RouteMatcher.OneOf =>
              oneOfMatch(paths, oneOf)
            case const: RouteMatcher.Const =>
              if (const.const == pHead)
                attemptMatch(pTail, const.child)
              else
                None
            case _: RouteMatcher.Complete =>
              None
            case pathArg: RouteMatcher.PathArg[_] =>
              val decoder: DecodeString[pathArg.Type] = pathArg.decodeString
              decoder.decode(pHead) match {
                case Alive(arg) =>
                  attemptMatch(pTail, pathArg.child(arg))
                case Dead(_) =>
                  None
              }
          }
        case Nil =>
          routeMatcher match {
            case oneOf: RouteMatcher.OneOf =>
              oneOfMatch(paths, oneOf)
            case complete: RouteMatcher.Complete =>
              // TODO (KR) : Possibly change how this works...
              complete.paramMatch(params) match {
                case Alive(r) => r.some
                case Dead(_)  => None
              }
            case _: RouteMatcher.Const =>
              None
            case _: RouteMatcher.PathArg[_] =>
              None
          }
      }

    attemptMatch(paths.tail, this) match {
      case Some(page) =>
        page.replaceNoTrace.runAndShowErrors()
      case None =>
        window.alert("Unable to resolve URL")
    }
  }

  private[pye] def bindToWindow(): Unit = {
    window.onpopstate = _ => {
      attemptToLoadPage()
    }
  }

}

object RouteMatcher {

  final case class Params(paramMap: Map[String, String]) {

    def param[P: DecodeString](name: String): ?[P] =
      for {
        mValue <- mParam[P](name)
        value <- mValue.toEA(Message(s"Missing param: $name"))
      } yield value
    def mParam[P: DecodeString](name: String): ?[Maybe[P]] =
      paramMap.get(name).toMaybe.map(implicitly[DecodeString[P]].decode).traverse

    def withParams(params: (String, String)*): Params =
      Params(paramMap ++ params)

    def withParam(key: String, value: String): Params =
      withParams(key -> value)

    def withMParam(key: String, value: Maybe[String]): Params =
      value.cata(withParam(key, _), this)

  }
  object Params {
    val empty: Params = Params(Map.empty)
  }

  // =====|  |=====

  final class OneOf private[RouteMatcher] (val children: List[RouteMatcher]) extends RouteMatcher
  final class Const private[RouteMatcher] (val const: String, val child: RouteMatcher) extends RouteMatcher
  final class Complete private[RouteMatcher] (val paramMatch: Params => ?[Page]) extends RouteMatcher
  final class PathArg[A] private[RouteMatcher] (val decodeString: DecodeString[A], val child: A => RouteMatcher)
      extends RouteMatcher {
    type Type = A
  }

  // =====|  |=====

  def oneOf(children: RouteMatcher*): RouteMatcher =
    new OneOf(children.toList)

  def const(const: String)(child: RouteMatcher): RouteMatcher =
    new Const(const, child)

  def complete(paramMatch: Params => ?[Page]): RouteMatcher =
    new Complete(paramMatch)

  def pathArg[A: DecodeString](child: A => RouteMatcher): RouteMatcher =
    new PathArg[A](implicitly[DecodeString[A]], child)

}
