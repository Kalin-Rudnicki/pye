package klib.webServer

import java.io.File

import io.circe._, parser._, generic.auto._
import jakarta.servlet.http.Cookie
import scalatags.Text.Frag

import klib.Implicits._
import klib.fp.types._

sealed trait MatchResult
object MatchResult {

  case object FailedMatch extends MatchResult

  final case class Response(
      body: String,
      code: Int,
      contentType: Maybe[String] = None,
      headers: Map[String, String],
      cookies: List[Cookie], // TODO (KR) : Correct?
  ) extends MatchResult {

    def withHeaders(headers: (String, String)*): Response =
      this.copy(
        headers = this.headers ++ headers,
      )

    def withCookies(cookies: Cookie*): Response =
      this.copy(
        cookies = this.cookies ++ cookies,
      )

  }

  object Response {

    def file(
        file: File,
        contentType: Maybe[String] = None,
        code: Int = 200,
    ): ??[MatchResult] =
      for {
        exists <- file.exists.pure[??]
        result <-
          if (exists)
            for {
              content <- IO.readFile(file).wrap
            } yield raw(content, contentType, code)
          else
            FailedMatch.pure[??] // TODO (KR) : better option? (aka: html(...))
      } yield result

    def raw(
        body: String,
        contentType: Maybe[String] = None,
        code: Int = 200,
    ): Response =
      Response(
        body = body,
        code = code,
        contentType = contentType,
        headers = Map.empty,
        cookies = Nil,
      )

    def json[J: Encoder](
        json: J,
        code: Int = 200,
    ): Response =
      raw(
        implicitly[Encoder[J]].apply(json).toString, // TODO (KR) : correct?
        "application/json".some,
        code,
      )

    def html(
        frag: Frag,
        code: Int = 200,
    ): Response =
      Response(
        body = frag.render, // TODO (KR) : correct?
        code = code,
        contentType = "text/html".some,
        headers = Map.empty,
        cookies = Nil,
      )

  }

}
