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

    private def fromMap[R: MatchData.DecodeString](label: String, map: Map[String, String], k: String): ?[R] =
      map
        .get(k)
        .toMaybe match {
        case Some(value) =>
          implicitly[MatchData.DecodeString[R]].decode(value)
        case None =>
          ?.dead(Message(s"Missing required $label '$k'"))
      }

    private def mFromMap[R: MatchData.DecodeString](map: Map[String, String], k: String): ?[Maybe[R]] =
      map
        .get(k)
        .toMaybe match {
        case Some(value) =>
          implicitly[MatchData.DecodeString[R]].decode(value).map(_.some)
        case None =>
          None.pure[?]
      }

    def param[P: MatchData.DecodeString](p: String): ?[P] =
      fromMap[P]("param", params, p)

    def mParam[P: MatchData.DecodeString](p: String): ?[Maybe[P]] =
      mFromMap[P](params, p)

    def header[H: MatchData.DecodeString](h: String): ?[H] =
      fromMap[H]("header", headers, h)

    def mHeader[H: MatchData.DecodeString](h: String): ?[Maybe[H]] =
      mFromMap[H](headers, h)

    def cookie[C: MatchData.DecodeString](c: String): ?[C] =
      fromMap[C]("cookie", cookies, c)

    def mCookie[C: MatchData.DecodeString](c: String): ?[Maybe[C]] =
      mFromMap[C](cookies, c)

    def cookieJson[C: Decoder](c: String): ?[C] =
      fromMap[C]("cookie", cookies, c)(MatchData.DecodeString.fromCirceDecoder[C])

    def mCookieJson[C: Decoder](c: String): ?[Maybe[C]] =
      mFromMap[C](cookies, c)(MatchData.DecodeString.fromCirceDecoder[C])

  }

  object MatchData {

    trait DecodeString[+T] {
      def decode(string: String): ?[T]
    }

    object DecodeString {

      def fromCirceDecoder[R: Decoder]: DecodeString[R] =
        s =>
          (
            for {
              json <- parse(s)
              decoded <- implicitly[Decoder[R]].decodeJson(json)
            } yield decoded
          ).toErrorAccumulator

      implicit val stringDecodeString: DecodeString[String] =
        _.pure[?]

      implicit val intDecodeString: DecodeString[Int] =
        i => i.toIntOption.toMaybe.toEA(Message(s"Malformatted int '$i'"))

    }

  }

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
