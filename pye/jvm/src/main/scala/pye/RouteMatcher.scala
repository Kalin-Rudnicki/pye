package pye

import io.circe._, parser._

import klib.Implicits._
import klib.fp.typeclass.DecodeString
import klib.fp.types._
import klib.utils._
import pye.db.ConnectionFactory

sealed trait RouteMatcher {

  def /:(const: String): RouteMatcher =
    RouteMatcher.const(const)(this)

}

object RouteMatcher {

  // =====|  |=====

  final class MatchData(
      val logger: Logger,
      val connectionFactory: ConnectionFactory,
      val body: IO[String],
      val bodyBytes: IO[Array[Byte]],
      val headers: Map[String, String],
      val params: Map[String, String],
      val cookies: Map[String, String],
  ) {

    def bodyAs[B: Decoder]: IO[B] =
      for {
        _body <- body
        res <- {
          for {
            json <- parse(_body).toErrorAccumulator: ?[Json]
            b <- implicitly[Decoder[B]].decodeJson(json).toErrorAccumulator
          } yield b
        }.toIO
      } yield res

    implicit private def decodeStringFromCirceDecoder[T](implicit decoder: Decoder[T]): DecodeString[T] =
      s =>
        (
          for {
            json <- parse(s)
            decoded <- decoder.decodeJson(json)
          } yield decoded
        ).toErrorAccumulator

    private def fromMap[R: DecodeString](label: String, map: Map[String, String], k: String): ?[R] =
      map
        .get(k)
        .toMaybe match {
        case Some(value) =>
          implicitly[DecodeString[R]].decode(value)
        case None =>
          ?.dead(Message(s"Missing required $label '$k'"))
      }

    private def mFromMap[R: DecodeString](map: Map[String, String], k: String): ?[Maybe[R]] =
      map
        .get(k)
        .toMaybe match {
        case Some(value) =>
          implicitly[DecodeString[R]].decode(value).map(_.some)
        case None =>
          None.pure[?]
      }

    def param[P: DecodeString](p: String): ?[P] =
      fromMap[P]("param", params, p)

    def mParam[P: DecodeString](p: String): ?[Maybe[P]] =
      mFromMap[P](params, p)

    def header[H: DecodeString](h: String): ?[H] =
      fromMap[H]("header", headers, h)

    def mHeader[H: DecodeString](h: String): ?[Maybe[H]] =
      mFromMap[H](headers, h)

    def headerJson[H: Decoder](h: String): ?[H] =
      fromMap[H]("header", headers, h)

    def mHeaderJson[H: Decoder](h: String): ?[Maybe[H]] =
      mFromMap[H](headers, h)

    def cookie[C: DecodeString](c: String): ?[C] =
      fromMap[C]("cookie", cookies, c)

    def mCookie[C: DecodeString](c: String): ?[Maybe[C]] =
      mFromMap[C](cookies, c)

    def cookieJson[C: Decoder](c: String): ?[C] =
      fromMap[C]("cookie", cookies, c)

    def mCookieJson[C: Decoder](c: String): ?[Maybe[C]] =
      mFromMap[C](cookies, c)

  }

  // =====|  |=====

  final class OneOf private[RouteMatcher] (val children: List[RouteMatcher]) extends RouteMatcher

  final class Const private[RouteMatcher] (val const: String, val child: RouteMatcher) extends RouteMatcher

  final class Complete private[RouteMatcher] (val toResult: MatchData => IO[Maybe[Response]]) extends RouteMatcher

  final class Any private[RouteMatcher] (
      val toResult: List[String] => MatchData => IO[Maybe[Response]],
  ) extends RouteMatcher

  final class Method private[RouteMatcher] (val method: String, val child: RouteMatcher) extends RouteMatcher

  final class PathArg[A] private[RouteMatcher] (val decodeString: DecodeString[A], val child: A => RouteMatcher)
      extends RouteMatcher {
    type Type = A
  }

  def any(toResult: List[String] => MatchData => IO[Maybe[Response]]): RouteMatcher =
    new Any(toResult)

  def complete(toResult: MatchData => IO[Maybe[Response]]): RouteMatcher =
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
