package klib.webServer

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

import io.circe._, parser._, generic.auto._
import jakarta.servlet.http.HttpServletRequest
import jakarta.servlet.http.HttpServletResponse
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler

import klib.Implicits._
import klib.fp.types._
import klib.utils._

final class ServerHandler(matcher: RouteMatcher, logger: Logger) extends AbstractHandler {

  override def handle(
      target: String,
      baseRequest: Request,
      request: HttpServletRequest,
      response: HttpServletResponse,
  ): Unit = {
    val method = request.getMethod
    val routes: List[String] = target.split("/").toList.filter(_.nonEmpty)
    val paramMap: ?[Map[String, String]] =
      request.getParameterMap.asScala.toList
        .map {
          case (k, v) =>
            v.toList match {
              case Nil =>
                (k, "").pure[?]
              case arg :: Nil =>
                (k, arg).pure[?]
              case _ =>
                Dead(Message(s"Malformatted param: ${k.unesc}") :: Nil)
            }
        }
        .traverse
        .map(_.toMap)
    lazy val body: String =
      request.getReader.lines().toArray().mkString("\n")

    def rec(
        params: Map[String, String],
        args: List[String],
        matcher: RouteMatcher,
    ): ??[MatchResult] =
      matcher match {
        case const: RouteMatcher.Const =>
          args match {
            case const.const :: tail =>
              rec(params, tail, const.child)
            case _ =>
              MatchResult.FailedMatch.pure[??]
          }
        case of: RouteMatcher.OneOf =>
          def inner(children: List[RouteMatcher]): ??[MatchResult] =
            children match {
              case head :: tail =>
                for {
                  res <- rec(params, args, head)
                  res2 <- res match {
                    case MatchResult.FailedMatch =>
                      inner(tail)
                    case res =>
                      res.pure[??]
                  }
                } yield res2
              case Nil =>
                MatchResult.FailedMatch.pure[??]
            }

          inner(of.children)
        case complete: RouteMatcher.Complete =>
          args match {
            case Nil if complete.method == method =>
              complete.toResult
            case _ =>
              MatchResult.FailedMatch.pure[??]
          }
        case withBody: RouteMatcher.WithBody[_] =>
          parse(body) match {
            case scala.Right(json) =>
              (withBody.decoder: Decoder[withBody.Type])
                .decodeJson(json) match {
                case scala.Right(value) =>
                  rec(params, args, withBody.child(value))
                case scala.Left(_) =>
                  MatchResult.FailedMatch.pure[??]
              }
            case scala.Left(_) =>
              MatchResult.FailedMatch.pure[??]
          }
        case pathArg: RouteMatcher.PathArg[_] =>
          args match {
            case head :: tail =>
              (pathArg.decodeString: RouteMatcher.DecodeString[pathArg.Type]).decode(head) match {
                case Some(arg) =>
                  rec(params, tail, pathArg.child(arg))
                case None =>
                  MatchResult.FailedMatch.pure[??]
              }
            case Nil =>
              MatchResult.FailedMatch.pure[??]
          }
        case withParam: RouteMatcher.WithParam[_] =>
          params.get(withParam.param) match {
            case scala.Some(arg) =>
              (withParam.decodeString: RouteMatcher.DecodeString[withParam.Type]).decode(arg) match {
                case Some(arg) =>
                  rec(params, args, withParam.child(arg))
                case None =>
                  MatchResult.FailedMatch.pure[??]
              }
            case scala.None =>
              MatchResult.FailedMatch.pure[??]
          }
      }

    (
      for {
        params <- paramMap.wrap[IO]
        res <- rec(params, routes, matcher)
      } yield res
    ).run match {
      case Alive(result, warnings) =>
        // TODO (KR) :
        ???
      case Dead(errors, warnings) =>
        // TODO (KR) :
        ???
    }
  }

}
