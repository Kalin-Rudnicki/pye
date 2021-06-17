package klib.webServer

import scala.concurrent.Future
import scala.concurrent.Promise

import io.circe._, parser._
import org.scalajs.dom._

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._

object HttpRequest {

  def apply(
      method: String,
      url: String,
  ): Stage1 =
    new Stage1(
      method = method,
      url = url,
      headers = Nil,
    )

  final class Stage1 private[HttpRequest] (
      method: String,
      url: String,
      headers: List[(String, String)],
  ) {

    def header[H](header: String, value: H)(implicit encoder: Encoder[H]): Stage1 =
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

    private def build[Response](f: (Int, String, Promise[Response]) => Unit): Future[Response] = {
      val promise: Promise[Response] = Promise()

      val xhr = new XMLHttpRequest()
      xhr.open(
        method = method,
        url = url,
        async = true,
      )
      headers.foreach(h => xhr.setRequestHeader(h._1, h._2))
      xhr.onload = { (_: Event) =>
        f(xhr.status, xhr.responseText, promise)
      }

      body match {
        case Some((body, encoder)) =>
          xhr.send(encoder.apply(body).toString)
        case None =>
          xhr.send()
      }

      promise.future
    }

    def raw: Future[(Int, String)] =
      build[(Int, String)] { (status, responseText, promise) =>
        promise.success((status, responseText))
      }

    def decodeResponse[Response](implicit decoder: DecodeString[Response]): Future[Response] =
      build[Response] { (status, responseText, promise) =>
        if (status == 200) {
          val mResponse = decoder.decode(responseText)

          mResponse match {
            case Alive(value, _) =>
              promise.success(value)
            case Dead(errors, _) =>
              promise.failure(Compound(errors))
          }
        } else {
          promise.failure(new RuntimeException(s"non-200-response ($status): $responseText"))
        }
      }

    def jsonResponse[Response](implicit decoder: Decoder[Response]): Future[Response] =
      build[Response] { (status, responseText, promise) =>
        if (status == 200) {
          val mResponse =
            for {
              json <- parse(responseText)
              response <- decoder.decodeJson(json)
            } yield response

          mResponse match {
            case util.Left(value) =>
              promise.failure(value)
            case util.Right(value) =>
              promise.success(value)
          }
        } else {
          promise.failure(new RuntimeException(s"non-200-response ($status): $responseText"))
        }
      }

  }

}
