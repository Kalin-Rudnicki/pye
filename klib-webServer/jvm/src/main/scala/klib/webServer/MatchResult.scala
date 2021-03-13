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

}

// =====|  |=====

sealed trait MatchResult2[+T]
object MatchResult2 {

  final case class Continue[+T](value: T) extends MatchResult2[T]
  final case class Done(result: Response) extends MatchResult2[Nothing]

}
