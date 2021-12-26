package pye

import scala.concurrent.Promise
import scala.scalajs.js
import scala.scalajs.js.URIUtils.encodeURIComponent

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
      baseUrl: String,
      requestErrorsAsJson: Boolean = true,
  ): Stage1 = {
    val r =
      new Stage1(
        method = method,
        baseUrl = baseUrl,
        paramList = Nil,
        headers = Nil,
      )

    if (requestErrorsAsJson)
      r.header("ERROR-TYPE", "json")
    else
      r
  }

  final class Stage1 private[HttpRequest] (
      method: String,
      baseUrl: String,
      paramList: List[(String, String)],
      headers: List[(String, String)],
  ) {

    def params(params: (String, String)*): Stage1 =
      new Stage1(
        method = method,
        baseUrl = baseUrl,
        paramList = params.toList ::: paramList,
        headers = headers,
      )

    def param(p: String, v: String): Stage1 =
      params(p -> v)

    def mParam(p: String, v: Maybe[String]): Stage1 =
      v match {
        case Some(v) =>
          param(p, v)
        case None =>
          this
      }

    def header(header: String, value: String): Stage1 =
      new Stage1(
        method = method,
        baseUrl = baseUrl,
        paramList = paramList,
        headers = (header, value) :: headers,
      )

    def headerJson[H](header: String, value: H)(implicit encoder: Encoder[H]): Stage1 =
      new Stage1(
        method = method,
        baseUrl = baseUrl,
        paramList = paramList,
        headers = (header, encoder.apply(value).toString) :: headers,
      )

    def noBody: Stage2 =
      new Stage2(
        method = method,
        baseUrl = baseUrl,
        body = None,
        params = paramList,
        headers = headers,
      )

    def rawBody(body: js.Any): Stage2 =
      new Stage2(
        method = method,
        baseUrl = baseUrl,
        body = body.some,
        params = paramList,
        headers = headers,
      )

    def jsonBody[Body](body: Body, jsonToString: Json => String = _.noSpaces)(implicit encoder: Encoder[Body]): Stage2 =
      new Stage2(
        method = method,
        baseUrl = baseUrl,
        body = Some(jsonToString(encoder.apply(body))),
        params = paramList,
        headers = headers,
      )

  }

  final class Stage2 private[HttpRequest] (
      method: String,
      baseUrl: String,
      body: Maybe[js.Any],
      params: List[(String, String)],
      headers: List[(String, String)],
  ) {

    private def buildResponse[Response](
        getResponse: XMLHttpRequest => ?[Response],
        responseType: Maybe[String] = None,
    ): AsyncIO[Response] = {
      val promise: Promise[?[Response]] = Promise()

      def encodeParam(p: (String, String)): String =
        s"${encodeURIComponent(p._1)}=${encodeURIComponent(p._2)}"

      val xhr = new XMLHttpRequest()
      xhr.open(
        method = method,
        url = s"$baseUrl${params.isEmpty ? "" | s"?${params.reverseMap(encodeParam).mkString("&")}"}",
        async = true,
      )
      headers.foreach(h => xhr.setRequestHeader(h._1, h._2))

      xhr.onload = { (_: Event) =>
        promise.success(getResponse(xhr))
      }

      responseType.foreach(xhr.responseType = _)
      body match {
        case Some(body) =>
          xhr.send(body)
        case None =>
          xhr.send()
      }

      AsyncIO.wrapWrappedEffect(_ => promise.future)
    }

    private def require200Response[Response](
        decodeResponse: XMLHttpRequest => ?[Response],
    )(xhr: XMLHttpRequest): ?[Response] =
      if (xhr.status == 200)
        decodeResponse(xhr)
      else
        decode[ErrorResponse](xhr.responseText).to_\/ match {
          case Right(errorResponse) => errorResponse.to_?
          case Left(_)              => ?.error(Message("Unable to decode intended response or server errors"))
        }

    def statusAndResponseText: AsyncIO[(Int, String)] =
      buildResponse[(Int, String)](xhr => (xhr.status, xhr.responseText).pure[?])

    def raw200: AsyncIO[String] =
      decodeResponse[String]

    def decodeResponse[Response: DecodeFromString]: AsyncIO[Response] =
      buildResponse[Response](
        require200Response[Response](xhr => DecodeFromString[Response].decode(xhr.responseText)),
      )

    def jsonResponse[Response: Decoder]: AsyncIO[Response] =
      decodeResponse[Response](DecodeFromString.fromCirceDecoder)

    def blobResponse: AsyncIO[Blob] =
      buildResponse[Blob](
        require200Response[Blob] { xhr =>
          xhr.response match {
            case blob: Blob => blob.pure[?]
            case _          => ?.dead(Message("Did not receive a blob"))
          }
        },
        "blob".some,
      )

  }

}
