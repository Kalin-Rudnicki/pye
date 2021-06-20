package klib.webServer

import scala.concurrent.Future
import scala.concurrent.Promise

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import org.scalajs.dom._

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._

object HttpRequest {

  def apply(
      method: String,
      url: String,
      requestErrorsAsJson: Boolean = true,
  ): Stage1 = {
    val r =
      new Stage1(
        method = method,
        url = url,
        headers = Nil,
      )

    if (requestErrorsAsJson)
      r.header("ERROR-TYPE", "json")
    else
      r
  }

  final class Stage1 private[HttpRequest] (
      method: String,
      url: String,
      headers: List[(String, String)],
  ) {

    def header(header: String, value: String): Stage1 =
      new Stage1(
        method = method,
        url = url,
        headers = (header, value) :: headers,
      )

    def headerJson[H](header: String, value: H)(implicit encoder: Encoder[H]): Stage1 =
      new Stage1(
        method = method,
        url = url,
        headers = (header, encoder.apply(value).toString) :: headers,
      )

    def noBody: Stage2[Nothing] =
      new Stage2(
        method = method,
        url = url,
        body = None,
        headers = headers,
      )

    def jsonBody[Body](body: Body)(implicit encoder: Encoder[Body]): Stage2[Body] =
      new Stage2(
        method = method,
        url = url,
        body = (body, encoder).some,
        headers = headers,
      )

  }

  final class Stage2[Body] private[HttpRequest] (
      method: String,
      url: String,
      body: Maybe[(Body, Encoder[Body])],
      headers: List[(String, String)],
  ) {

    private def build[Response](f: (Int, String) => ?[Response]): HttpResponse[Response] = {
      val promise: Promise[?[Response]] = Promise()

      val xhr = new XMLHttpRequest()
      xhr.open(
        method = method,
        url = url,
        async = true,
      )
      headers.foreach(h => xhr.setRequestHeader(h._1, h._2))
      xhr.onload = { (_: Event) =>
        promise.success(f(xhr.status, xhr.responseText))
      }

      body match {
        case Some((body, encoder)) =>
          xhr.send(encoder.apply(body).toString)
        case None =>
          xhr.send()
      }

      HttpResponse(promise.future)
    }

    def raw: HttpResponse[(Int, String)] =
      build[(Int, String)] { (status, responseText) =>
        (status, responseText).pure[?]
      }

    def decodeResponse[Response](implicit decoder: DecodeString[Response]): HttpResponse[Response] =
      build[Response] { (_, responseText) =>
        decoder.decode(responseText) match {
          case alive: Alive[Response] =>
            alive
          case Dead(errors) =>
            decode[ErrorResponse](responseText).to_\/ match {
              case Right(r) =>
                r.to_?
              case Left(error) =>
                Dead(errors.appended(error))
            }
        }
      }

    def jsonResponse[Response](implicit decoder: Decoder[Response]): HttpResponse[Response] =
      build[Response] { (_, responseText) =>
        for {
          json <- parse(responseText).toErrorAccumulator: ?[Json]
          response <- decoder.decodeJson(json).toErrorAccumulator match {
            case alive @ Alive(_) =>
              alive
            case Dead(errors) =>
              implicitly[Decoder[ErrorResponse]].decodeJson(json).to_\/ match {
                case Right(b) =>
                  b.to_?
                case Left(error) =>
                  Dead(errors.appended(error))
              }
          }
        } yield response
      }

  }

}
