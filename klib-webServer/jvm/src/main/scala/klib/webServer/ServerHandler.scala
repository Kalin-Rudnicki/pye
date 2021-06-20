package klib.webServer

import scala.jdk.CollectionConverters._

import jakarta.servlet.http.HttpServletRequest
import jakarta.servlet.http.HttpServletResponse
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler
import scalatags.Text.all.{body => htmlBody, _}

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import klib.utils.Logger.{helpers => L}
import klib.utils.Logger.helpers.Implicits._
import klib.fp.typeclass.DecodeString
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
    ): ??[Maybe[Response]] =
      matcher match {
        case const: RouteMatcher.Const =>
          args match {
            case const.const :: tail =>
              rec(params, tail, const.child)
            case _ =>
              None.pure[??]
          }
        case of: RouteMatcher.OneOf =>
          def inner(children: List[RouteMatcher]): ??[Maybe[Response]] =
            children match {
              case head :: tail =>
                for {
                  res <- rec(params, args, head)
                  res2 <- res match {
                    case some @ Some(_) =>
                      some.pure[??]
                    case None =>
                      inner(tail)
                  }
                } yield res2
              case Nil =>
                None.pure[??]
            }

          inner(of.children)
        case _method: RouteMatcher.Method =>
          if (_method.method == method)
            rec(params, args, _method.child)
          else
            None.pure[??]
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
              None.pure[??]
          }
        case pathArg: RouteMatcher.PathArg[_] =>
          args match {
            case head :: tail =>
              pathArg.decodeString.decode(head) match {
                case Alive(arg: pathArg.Type) =>
                  rec(params, tail, pathArg.child(arg))
                case _ =>
                  None.pure[??]
              }
            case Nil =>
              None.pure[??]
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
              params <- paramMap.to_??
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
              ).to_??
              _ = println("1.1")
              res <- rec(params, routes, matcher)
              _ = println("1.2")
            } yield res
        ).runSync.pure[??]
        _ = println("2.1")
        _ <- matchResult match {
          case Alive(result) =>
            result match {
              case Some(r) =>
                writeResult(r).to_??
              case None =>
                writeResult(
                  Response.html(
                    htmlFromBody(
                      h1("Couldn't find what you were looking for (klib-webserver)"),
                    ),
                    code = Response.Code.NotFound,
                  ),
                ).to_??
            }
          case dead @ Dead(errors) =>
            for {
              _ <- logger(errors.map(L.log.throwable(_))).to_??
              _ <-
                if (isTestEnv)
                  writeResult(
                    headers.get("ERROR-TYPE").toMaybe match {
                      case Some("json") =>
                        import io.circe.generic.auto._
                        println("3.1.1")
                        Response.json(
                          ErrorResponse.fromDead(dead),
                          code = Response.Code.InternalServerError,
                        )
                      case _ =>
                        println("3.1.2")
                        Response.html(
                          errorHtml(errors),
                          code = Response.Code.InternalServerError,
                        )
                    },
                  ).to_??
                else
                  // TODO (KR) :
                  ???
            } yield ()
        }
        _ = println("Setting Handled")
        _ <- baseRequest.setHandled(true).pure[??]
        _ = println(s"Set to handled: ${baseRequest.isHandled}")
      } yield ()
    ).runSync
  }

}
