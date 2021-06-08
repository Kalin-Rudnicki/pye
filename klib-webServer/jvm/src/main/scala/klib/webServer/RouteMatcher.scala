package klib.webServer

import io.circe._, parser._
import jakarta.servlet.http.Cookie

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import klib.webServer.db.ConnectionFactory

sealed trait RouteMatcher {

  def /:(const: String): RouteMatcher =
    RouteMatcher.const(const)(this)

}

object RouteMatcher {

  trait DecodeString[T] {
    def decode(string: String): Maybe[T]
  }

  object DecodeString {

    implicit val stringDecodeString: DecodeString[String] = _.some

    implicit val intDecodeString: DecodeString[Int] = _.toIntOption.toMaybe

    implicit val longDecodeString: DecodeString[Long] = _.toLongOption.toMaybe

  }

  // =====|  |=====

  final class MatchData(
      val logger: Logger,
      val connectionFactory: ConnectionFactory,
      val body: String, // TODO (KR) : Might need ??[_]
      val headers: Map[String, String],
      val params: Map[String, String],
      val cookies: Map[String, String],
  ) {

    def bodyAs[B: Decoder]: ??[B] =
      (
        for {
          json <- parse(body).to_\/.toMaybe
          b <- implicitly[Decoder[B]].decodeJson(json).to_\/.toMaybe
        } yield b
      ) match {
        case Some(b) =>
          b.pure[??]
        case None =>
          ??.dead(Message("Failed to decode body"))
      }

    // TODO (KR) : more helpers

  }

  object MatchData {}

  // =====|  |=====

  final class OneOf private[RouteMatcher] (val children: List[RouteMatcher]) extends RouteMatcher

  final class Const private[RouteMatcher] (val const: String, val child: RouteMatcher) extends RouteMatcher

  final class Complete private[RouteMatcher] (val toResult: MatchData => ??[MatchResult[Nothing]]) extends RouteMatcher

  final class Any private[RouteMatcher] (
      val toResult: (List[String], Map[String, String]) => MatchData => ??[MatchResult[Nothing]],
  ) extends RouteMatcher

  final class Method private[RouteMatcher] (val method: String, val child: RouteMatcher) extends RouteMatcher

  final class PathArg[A] private[RouteMatcher] (val decodeString: DecodeString[A], val child: A => RouteMatcher)
      extends RouteMatcher {
    type Type = A
  }

  def any(toResult: (List[String], Map[String, String]) => MatchData => ??[MatchResult[Nothing]]): RouteMatcher =
    new Any(toResult)

  def complete(toResult: MatchData => ??[MatchResult[Nothing]]): RouteMatcher =
    new Complete(toResult)

  def const(const: String)(child: RouteMatcher): RouteMatcher =
    new Const(const, child)

  def oneOf(children: RouteMatcher*): RouteMatcher =
    new OneOf(children.toList)

  def method(method: String)(child: RouteMatcher): RouteMatcher =
    new Method(method, child)

  def pathArg[A: DecodeString](child: A => RouteMatcher): RouteMatcher =
    new PathArg(implicitly[DecodeString[A]], child)

}
