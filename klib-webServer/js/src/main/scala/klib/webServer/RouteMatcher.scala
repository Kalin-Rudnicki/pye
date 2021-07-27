package klib.webServer

import scala.annotation.tailrec

import org.scalajs.dom._
import org.scalajs.dom.experimental.URLSearchParams

import klib.Implicits._
import klib.fp.typeclass.DecodeString
import klib.fp.types._

sealed trait RouteMatcher {

  def /:(const: String): RouteMatcher =
    RouteMatcher.const(const)(this)

  def attemptToLoadPage(): Unit = {
    val paths =
      window.location.pathname
        .split("/")
        .toList
        .filter(_.nonEmpty)
    val params =
      new URLSearchParams(window.location.search).toList
        .map { t =>
          (t._1, t._2)
        }
        .sortBy(_._1)

    def attemptMatch(
        paths: List[String],
        routeMatcher: RouteMatcher,
    ): Maybe[Page[_]] =
      paths match {
        case pHead :: pTail =>
          routeMatcher match {
            case of: RouteMatcher.OneOf =>
              @tailrec
              def loop(children: List[RouteMatcher]): Maybe[Page[_]] =
                children match {
                  case cHead :: cTail =>
                    attemptMatch(paths, cHead) match {
                      case None           => loop(cTail)
                      case some @ Some(_) => some
                    }
                  case Nil =>
                    None
                }

              loop(of.children)
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
            case complete: RouteMatcher.Complete =>
              complete.paramMatch.lift(params).toMaybe
            case _ =>
              None
          }
      }

    attemptMatch(paths.tail, this) match {
      case Some(page) =>
        page.replaceNoTrace()
      case None =>
        window.alert("Unable to resolve URL")
    }
  }

  def bindToWindow(): Unit = {
    window.onpopstate = _ => {
      attemptToLoadPage()
    }
  }

}

object RouteMatcher {

  // =====|  |=====

  final class OneOf private[RouteMatcher] (val children: List[RouteMatcher]) extends RouteMatcher
  final class Const private[RouteMatcher] (val const: String, val child: RouteMatcher) extends RouteMatcher
  final class Complete private[RouteMatcher] (val paramMatch: PartialFunction[List[(String, String)], Page[_]])
      extends RouteMatcher
  final class PathArg[A] private[RouteMatcher] (val decodeString: DecodeString[A], val child: A => RouteMatcher)
      extends RouteMatcher {
    type Type = A
  }

  // =====|  |=====

  def oneOf(children: RouteMatcher*): RouteMatcher =
    new OneOf(children.toList)

  def const(const: String)(child: RouteMatcher): RouteMatcher =
    new Const(const, child)

  def complete(paramMatch: PartialFunction[List[(String, String)], Page[_]]): RouteMatcher =
    new Complete(paramMatch)

  def pathArg[A: DecodeString](child: A => RouteMatcher): RouteMatcher =
    new PathArg[A](implicitly[DecodeString[A]], child)

}
