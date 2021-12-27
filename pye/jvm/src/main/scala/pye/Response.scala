package pye

import java.io.File

import io.circe._, parser._, generic.auto._
import jakarta.servlet.http.Cookie
import scalatags.Text.Frag

import klib.Implicits._
import klib.fp.types._

sealed trait Response

object Response {

  final case class Redirect(to: String) extends Response

  sealed trait Body
  object Body {
    final case class Bytes(bytes: Array[Byte]) extends Body
    final case class File(file: java.io.File) extends Body
  }

  final case class Data(
      body: Body,
      code: Response.Code,
      contentType: Maybe[String] = None,
      headers: Map[String, String],
      cookies: List[Cookie],
  ) extends Response {

    def withHeaders(headers: (String, String)*): Data =
      this.copy(
        headers = this.headers ++ headers,
      )

    def withCookies(cookies: Cookie*): Data =
      this.copy(
        cookies = this.cookies ++ cookies,
      )

  }

  def file(
      file: File,
      contentType: Maybe[String] = None,
      code: Response.Code = Response.Code.OK,
  ): Data =
    Data(
      body = Body.File(file),
      code = code,
      contentType = contentType,
      headers = Map.empty,
      cookies = Nil,
    )

  def raw(
      body: Array[Byte],
      contentType: Maybe[String] = None,
      code: Response.Code = Response.Code.OK,
  ): Data =
    Data(
      body = Body.Bytes(body),
      code = code,
      contentType = contentType,
      headers = Map.empty,
      cookies = Nil,
    )

  def text(
      body: String,
      contentType: Maybe[String] = None,
      code: Response.Code = Response.Code.OK,
  ): Data =
    raw(
      body = body.getBytes,
      code = code,
      contentType = contentType,
    )

  def json[J: Encoder](
      json: J,
      code: Response.Code = Response.Code.OK,
      jsonToString: Json => String = _.noSpaces,
  ): Data =
    text(
      jsonToString(Encoder[J].apply(json)),
      "application/json".some,
      code,
    )

  def html(
      frag: Frag,
      code: Response.Code = Response.Code.OK,
  ): Data =
    text(
      body = frag.render,
      code = code,
      contentType = "text/html".some,
    )

  // =====|  |=====

  final class Code private (val code: Int)
  object Code {
    private def apply(code: Int): Code =
      new Code(code)

    // --- By Name ---
    val Continue: Code = Code(100)
    val SwitchingProtocol: Code = Code(101)
    val Processing: Code = Code(102)
    val EarlyHints: Code = Code(103)

    val OK: Code = Code(200)
    val Created: Code = Code(201)
    val Accepted: Code = Code(202)
    val NonAuthoritativeInformation: Code = Code(203)
    val NoContent: Code = Code(204)
    val ResetContent: Code = Code(205)
    val PartialContent: Code = Code(206)
    val MultiStatus: Code = Code(207)
    val AlreadyReported: Code = Code(208)
    val IMUsed: Code = Code(226)

    val MultipleChoice: Code = Code(300)
    val MovedPermanently: Code = Code(301)
    val Found: Code = Code(302)
    val SeeOther: Code = Code(303)
    val NotModified: Code = Code(304)
    val UseProxy: Code = Code(305)
    val Unused: Code = Code(306)
    val TemporaryRedirect: Code = Code(307)
    val PermanentRedirect: Code = Code(308)

    val BadRequest: Code = Code(400)
    val Unauthorized: Code = Code(401)
    val PaymentRequired: Code = Code(402)
    val Forbidden: Code = Code(403)
    val NotFound: Code = Code(404)
    val MethodNotAllowed: Code = Code(405)
    val NotAcceptable: Code = Code(406)
    val ProxyAuthenticationRequired: Code = Code(407)
    val RequestTimeout: Code = Code(408)
    val Conflict: Code = Code(409)
    val Gone: Code = Code(410)
    val LengthRequired: Code = Code(411)
    val PreconditionFailed: Code = Code(412)
    val PayloadTooLarge: Code = Code(413)
    val UriTooLong: Code = Code(414)
    val UnsupportedMediaType: Code = Code(415)
    val RangeNotSatisfiable: Code = Code(416)
    val ExpectationFailed: Code = Code(417)
    val ImATeapot: Code = Code(418)
    val MisdirectedRequest: Code = Code(421)
    val UnprocessableEntity: Code = Code(422)
    val Locked: Code = Code(423)
    val FailedDependency: Code = Code(424)
    val TooEarly: Code = Code(425)
    val UpgradeRequired: Code = Code(426)
    val PreconditionRequired: Code = Code(428)
    val TooManyRequests: Code = Code(429)
    val RequestHeaderFieldsTooLarge: Code = Code(431)
    val UnavailableForLegalReasons: Code = Code(451)

    val InternalServerError: Code = Code(500)
    val NotImplemented: Code = Code(501)
    val BadGateway: Code = Code(502)
    val ServiceUnavailable: Code = Code(503)
    val GatewayTimeout: Code = Code(504)
    val HttpVersionNotSupported: Code = Code(505)
    val VariantAlsoNegotiates: Code = Code(506)
    val InsufficientStorage: Code = Code(507)
    val LoopDetected: Code = Code(508)
    val NotExtended: Code = Code(510)
    val NetworkAuthenticationRequired: Code = Code(511)

  }

}
