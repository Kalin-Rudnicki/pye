package pye

import java.io.File
import java.io.FileOutputStream

import io.circe._
import parser._

import klib.Implicits._
import klib.fp.typeclass.DecodeFromString
import klib.fp.types._
import klib.utils._
import pye.db.{ConnectionFactory, Connection}

sealed trait RouteMatcher {

  def /:(const: String): RouteMatcher =
    RouteMatcher.const(const)(this)

}

object RouteMatcher {

  // =====|  |=====

  final class MatchData(
      val logger: Logger,
      val connectionFactory: ConnectionFactory,
      val body: MatchData.Body,
      val headers: Map[String, String],
      val params: Map[String, String],
      val cookies: Map[String, String],
  ) {

    implicit private def decodeStringFromCirceDecoder[T](implicit decoder: Decoder[T]): DecodeFromString[T] =
      s =>
        (
          for {
            json <- parse(s)
            decoded <- decoder.decodeJson(json)
          } yield decoded
        ).toErrorAccumulator

    private def fromMap[R: DecodeFromString](label: String, map: Map[String, String], k: String): ?[R] =
      map
        .get(k)
        .toMaybe match {
        case Some(value) =>
          implicitly[DecodeFromString[R]].decode(value)
        case None =>
          ?.dead(Message(s"Missing required $label '$k'"))
      }

    private def mFromMap[R: DecodeFromString](map: Map[String, String], k: String): ?[Maybe[R]] =
      map
        .get(k)
        .toMaybe match {
        case Some(value) =>
          implicitly[DecodeFromString[R]].decode(value).map(_.some)
        case None =>
          None.pure[?]
      }

    def param[P: DecodeFromString](p: String): ?[P] =
      fromMap[P]("param", params, p)

    def mParam[P: DecodeFromString](p: String): ?[Maybe[P]] =
      mFromMap[P](params, p)

    def header[H: DecodeFromString](h: String): ?[H] =
      fromMap[H]("header", headers, h)

    def mHeader[H: DecodeFromString](h: String): ?[Maybe[H]] =
      mFromMap[H](headers, h)

    def headerJson[H: Decoder](h: String): ?[H] =
      fromMap[H]("header", headers, h)

    def mHeaderJson[H: Decoder](h: String): ?[Maybe[H]] =
      mFromMap[H](headers, h)

    def cookie[C: DecodeFromString](c: String): ?[C] =
      fromMap[C]("cookie", cookies, c)

    def mCookie[C: DecodeFromString](c: String): ?[Maybe[C]] =
      mFromMap[C](cookies, c)

    def cookieJson[C: Decoder](c: String): ?[C] =
      fromMap[C]("cookie", cookies, c)

    def mCookieJson[C: Decoder](c: String): ?[Maybe[C]] =
      mFromMap[C](cookies, c)

  }

  object MatchData {

    final class Body private[pye] (inputStream: => jakarta.servlet.ServletInputStream) {

      // NOTE : This method assumes you arent receiving some sort of massive input
      def raw: IO[String] =
        asBytes.map(new String(_))

      // NOTE : This method assumes you arent receiving some sort of massive input
      def asBytes: IO[Array[Byte]] =
        inputStream.readAllBytes.pure[IO]

      // NOTE : This method assumes you arent receiving some sort of massive input
      def decodeJson[J: Decoder]: IO[J] =
        for {
          _body <- raw
          res <- {
            for {
              json <- parse(_body).toErrorAccumulator: ?[Json]
              b <- implicitly[Decoder[J]].decodeJson(json).toErrorAccumulator
            } yield b
          }.toIO
        } yield res

      // NOTE : This method assumes you arent receiving some sort of massive input
      def decodeString[J: DecodeFromString]: IO[J] =
        for {
          _body <- raw
          res <- implicitly[DecodeFromString[J]].decode(_body).toIO
        } yield res

      // NOTE : This method does not care if you receive some sort of massive input
      def transferToFile(file: File): IO[Long] =
        for {
          outputStream <- IO { new FileOutputStream(file) }
          transferred <- inputStream.transferTo(outputStream).pure[IO]
        } yield transferred

    }

  }

  // =====|  |=====

  final class OneOf private[RouteMatcher] (val children: List[RouteMatcher]) extends RouteMatcher

  final class Const private[RouteMatcher] (val const: String, val child: RouteMatcher) extends RouteMatcher

  final class Complete private[RouteMatcher] (val toResult: MatchData => IO[Maybe[Response]]) extends RouteMatcher

  final class Any private[RouteMatcher] (
      val toResult: List[String] => MatchData => IO[Maybe[Response]],
  ) extends RouteMatcher

  final class Method private[RouteMatcher] (val method: String, val child: RouteMatcher) extends RouteMatcher

  final class PathArg[A] private[RouteMatcher] (val decodeString: DecodeFromString[A], val child: A => RouteMatcher)
      extends RouteMatcher {
    type Type = A
  }

  def any(toResult: List[String] => MatchData => IO[Maybe[Response]]): RouteMatcher =
    new Any(toResult)

  def complete(toResult: MatchData => IO[Maybe[Response]]): RouteMatcher =
    new Complete(toResult)

  def completeWithConnection(toResult: MatchData => Connection => IO[Maybe[Response]]): RouteMatcher =
    complete { md =>
      md.connectionFactory.withConnection { c =>
        toResult(md)(c)
      }
    }

  def const(const: String)(child: RouteMatcher): RouteMatcher =
    new Const(const, child)

  def oneOf(children: RouteMatcher*): RouteMatcher =
    new OneOf(children.toList)

  def method(method: String)(child: RouteMatcher): RouteMatcher =
    new Method(method, child)

  def pathArg[A: DecodeFromString](child: A => RouteMatcher): RouteMatcher =
    new PathArg(implicitly[DecodeFromString[A]], child)

}
