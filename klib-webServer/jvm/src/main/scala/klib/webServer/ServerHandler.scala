package klib.webServer

import scala.jdk.CollectionConverters._

import jakarta.servlet.http.HttpServletRequest
import jakarta.servlet.http.HttpServletResponse
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler
import scalatags.Text.all.{body => htmlBody, _}

import klib.Implicits._
import klib.fp.types._
import klib.utils._, Logger.{helpers => L}, L.Implicits._
import klib.webServer.db.ConnectionFactory

final class ServerHandler(
    matcher: RouteMatcher,
    connectionFactory: ConnectionFactory,
    logger: Logger,
) extends AbstractHandler {
  // TODO (KR) : Pass as param instead
  private val isTestEnv: Boolean = true

  override def handle(
      target: String,
      baseRequest: Request,
      request: HttpServletRequest,
      response: HttpServletResponse,
  ): Unit = {
    val method = request.getMethod
    val routes: List[String] = target.split("/").toList.filter(_.nonEmpty)
    val cookies: Map[String, String] =
      Maybe(request.getCookies).cata(
        _.toList.map { c =>
          (
            c.getName,
            c.getValue,
          )
        }.toMap,
        Map.empty,
      )
    val headers: Map[String, String] =
      Maybe(request.getHeaderNames).cata(
        _.asScala.toList.map(h => (h, request.getHeader(h))).toMap,
        Map.empty,
      )
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
    ): ??[MatchResult[Unit]] =
      matcher match {
        case const: RouteMatcher.Const =>
          args match {
            case const.const :: tail =>
              rec(params, tail, const.child)
            case _ =>
              MatchResult.Continue(()).pure[??]
          }
        case of: RouteMatcher.OneOf =>
          def inner(children: List[RouteMatcher]): ??[MatchResult[Unit]] =
            children match {
              case head :: tail =>
                for {
                  res <- rec(params, args, head)
                  res2 <- res match {
                    case MatchResult.Continue(_) =>
                      inner(tail)
                    case res: MatchResult.Done =>
                      res.pure[??]
                  }
                } yield res2
              case Nil =>
                MatchResult.Continue(()).pure[??]
            }

          inner(of.children)
        case _method: RouteMatcher.Method =>
          if (_method.method == method)
            rec(params, args, _method.child)
          else
            MatchResult.Continue(()).pure[??]
        case any: RouteMatcher.Any =>
          any.toResult(args, params)(
            new RouteMatcher.MatchData(
              logger = logger,
              connectionFactory = connectionFactory,
              body = body,
              headers = headers,
              params = params,
              cookies = cookies,
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
                  headers = headers,
                  params = params,
                  cookies = cookies,
                ),
              )
            case _ =>
              MatchResult.Continue(()).pure[??]
          }
        case pathArg: RouteMatcher.PathArg[_] =>
          args match {
            case head :: tail =>
              (pathArg.decodeString: RouteMatcher.DecodeString[pathArg.Type]).decode(head) match {
                case Some(arg) =>
                  rec(params, tail, pathArg.child(arg))
                case None =>
                  MatchResult.Continue(()).pure[??]
              }
            case Nil =>
              MatchResult.Continue(()).pure[??]
          }
      }

    def writeResult(r: Response): IO[Unit] =
      IO {
        response.setStatus(r.code.code)
        r.contentType.foreach(response.setContentType)
        response.getOutputStream.write(r.body)
        r.headers.foreach { case (key, value) => response.setHeader(key, value) }
        r.cookies.foreach(response.addCookie)
      }

    def htmlFromBody(bodies: Frag*): Frag =
      html(
        head(),
        htmlBody(
          bodies,
        ),
      )

    def errorHtml(errors: List[Throwable]): Frag =
      htmlFromBody(
        div(
          h1(s"Server Error(s): (${errors.size})"),
        ),
        errors.map { error =>
          div(
            h3(error.getMessage),
            div(
              // TODO (KR) :
              Logger.IgnoreStackTraceElement.trimmedTrace(error, Nil).map { st =>
                span(
                  s"> ${st.toString}",
                  br,
                  "\n",
                )
              },
            ),
            br,
          )
        },
      )

    (
      for {
        matchResult <- (
            for {
              params <- paramMap.wrap[IO]
              _ <- logger(
                L(
                  L.log.debug("--- Request ---"),
                  L.log.debug(s"Method: ${request.getMethod}"),
                  L.log.debug(s"Route: ${routes.mkString("/")}"),
                  L.log.debug(s"Cookies (${cookies.size}):"),
                  L.indented(
                    cookies.values.toList.map(L.log.debug),
                  ),
                  L.log.debug(s"Headers (${headers.size}):"),
                  L.indented(
                    headers.toList.map(p => L.log.debug(s"${p._1} => ${p._2}")),
                  ),
                  L.log.debug(s"Params (${params.size}):"),
                  L.indented(
                    params.toList.map(p => L.log.debug(s"${p._1} => ${p._2}")),
                  ),
                  // TODO (KR) : Other stuff?
                  L.break(),
                ),
              ).wrap
              res <- rec(params, routes, matcher)
            } yield res
        ).run.pure[??]
        _ <- matchResult match {
          // TODO (KR) : Maybe do something with warnings? (log?)
          case Alive(result, _) =>
            result match {
              case MatchResult.Continue(_) =>
                writeResult(
                  Response.html(
                    htmlFromBody(
                      h1("Couldn't find what you were looking for"),
                    ),
                    code = Response.Code.NotFound,
                  ),
                ).wrap
              case MatchResult.Done(r) =>
                writeResult(r).wrap
            }
          case dead @ Dead(errors, _) =>
            for {
              _ <- logger(errors.map(L.log.throwable(_))).wrap
              _ <-
                if (isTestEnv)
                  writeResult(
                    headers.get("ERROR-TYPE").toMaybe match {
                      case Some("json") =>
                        import io.circe.generic.auto._
                        Response.json(
                          ErrorResponse.fromDead(dead),
                          code = Response.Code.InternalServerError,
                        )
                      case _ =>
                        Response.html(
                          errorHtml(errors),
                          code = Response.Code.InternalServerError,
                        )
                    },
                  ).wrap
                else
                  // TODO (KR) :
                  ???
            } yield ()
        }
        _ <- baseRequest.setHandled(true).pure[??]
      } yield ()
    ).run
  }

}
