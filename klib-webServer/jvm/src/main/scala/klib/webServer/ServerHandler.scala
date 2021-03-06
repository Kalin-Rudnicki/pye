package klib.webServer

import scala.jdk.CollectionConverters._

import io.circe._, parser._
import jakarta.servlet.http.HttpServletRequest
import jakarta.servlet.http.HttpServletResponse
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import klib.webServer.db.ConnectionFactory

final class ServerHandler(
    matcher: RouteMatcher,
    connectionFactory: ConnectionFactory,
    logger: Logger,
    // TODO (KR) : (testEnv: Boolean)
) extends AbstractHandler {

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
        case _method: RouteMatcher.Method =>
          if (_method.method == method)
            rec(params, args, _method.child)
          else
            MatchResult.FailedMatch.pure[??]
        case any: RouteMatcher.Any =>
          any.toResult(args, params)(
            new RouteMatcher.MatchData(
              logger = logger,
              connectionFactory = connectionFactory,
              body = body,
              params = params,
              cookies = request.getCookies,
            ),
          )
        case complete: RouteMatcher.Complete =>
          args match {
            case Nil =>
              complete.toResult(
                new RouteMatcher.MatchData(
                  logger = logger,
                  connectionFactory = connectionFactory,
                  body = body,
                  params = params,
                  cookies = request.getCookies,
                ),
              )
            case _ =>
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
      }

    (
      for {
        _ <- logger() { src =>
          src.debug("--- Request ---")
          src.debug(s"Route: ${routes.mkString("/")}")
          // TODO (KR) : Other stuff?
          src.break
        }.wrap
        params <- paramMap.wrap[IO]
        res <- rec(params, routes, matcher)
      } yield res
    ).run match {
      // TODO (KR) : Maybe do something with warnings? (log?)
      case Alive(result, _) =>
        result match {
          case MatchResult.FailedMatch =>
            // TODO (KR) :
            ???
          case MatchResult.Response(body, code, contentType, headers, cookies) =>
            response.setStatus(code)
            contentType.forEach(response.setContentType)
            response.getWriter.write(body)
            headers.foreach { case (key, value) => response.setHeader(key, value) }
            cookies.foreach(response.addCookie)
        }
      case Dead(errors, _) =>
        // TODO (KR) :
        ???
    }
    baseRequest.setHandled(true)
  }

}
