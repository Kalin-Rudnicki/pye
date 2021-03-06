package klib.webServer

import io.circe._
import generic.auto._
import parser._

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

  final class Const private[RouteMatcher] (val const: String, val child: RouteMatcher) extends RouteMatcher

  final class OneOf private[RouteMatcher] (val children: List[RouteMatcher]) extends RouteMatcher

  final class Complete private[RouteMatcher] (val toResult: (ConnectionFactory, Logger) => ??[MatchResult])
      extends RouteMatcher

  final class Any private[RouteMatcher] (
      val toResult: (List[String], Map[String, String]) => (ConnectionFactory, Logger) => ??[MatchResult],
  ) extends RouteMatcher

  final class Method private[RouteMatcher] (val method: String, val child: RouteMatcher) extends RouteMatcher

  final class WithBody[B] private[RouteMatcher] (val decoder: Decoder[B], val child: B => RouteMatcher)
      extends RouteMatcher {
    type Type = B
  }

  final class PathArg[A] private[RouteMatcher] (val decodeString: DecodeString[A], val child: A => RouteMatcher)
      extends RouteMatcher {
    type Type = A
  }

  final class WithParam[A] private[RouteMatcher] (
      val decodeString: DecodeString[A],
      val param: String,
      val child: A => RouteMatcher,
  ) extends RouteMatcher {
    type Type = A
  }

  def any(toResult: (List[String], Map[String, String]) => (ConnectionFactory, Logger) => ??[MatchResult]): RouteMatcher =
    new Any(toResult)

  def complete(toResult: (ConnectionFactory, Logger) => ??[MatchResult]): RouteMatcher =
    new Complete(toResult)

  def const(const: String)(child: RouteMatcher): RouteMatcher =
    new Const(const, child)

  def oneOf(children: RouteMatcher*): RouteMatcher =
    new OneOf(children.toList)

  def method(method: String)(child: RouteMatcher): RouteMatcher =
    new Method(method, child)

  def withBody[B: Decoder](child: B => RouteMatcher): RouteMatcher =
    new WithBody(implicitly[Decoder[B]], child)

  def pathArg[A: DecodeString](child: A => RouteMatcher): RouteMatcher =
    new PathArg(implicitly[DecodeString[A]], child)

  def withParam[A: DecodeString](param: String)(child: A => RouteMatcher): RouteMatcher =
    new WithParam(implicitly[DecodeString[A]], param, child)

}
